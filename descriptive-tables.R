

## Plotting functions ----

plot_population = function(
  linelist,
  populationVar = "genomic.variant",
  comparisonVars = dplyr::vars(demog.age,demog.gender,demog.ethnicity),
  na.rm=FALSE
) {
  populationVar = rlang::ensym(populationVar)
  linelist = linelist %>% dplyr::ungroup()

  lapply(comparisonVars, function(comp) {

    if (linelist %>% dplyr::pull(!!comp) %>% is.numeric()) {

      ggplot2::ggplot(linelist, ggplot2::aes(x=!!comp, color=!!populationVar,fill=!!populationVar))+
        ggplot2::geom_density(alpha=0.2)+
        ggplot2::scale_color_brewer(palette = "Dark2",name="",aesthetics = c("fill","colour"))+
        ggplot2::xlab(readable_label(!!comp))

    } else if (linelist %>% dplyr::pull(!!comp) %>% is.factor()) {

      if (na.rm) {
        linelist = linelist %>% dplyr::filter(!is.na(!!comp))
      } else {
        linelist = linelist %>% dplyr::mutate(!!comp := forcats::fct_explicit_na(!!comp, "<missing>"))
      }

      linelist = linelist %>% dplyr::mutate(
        !!populationVar := forcats::fct_drop(!!populationVar),
        !!comp := forcats::fct_drop(!!comp)
      )

      compare_count = linelist %>%
        dplyr::group_by(!!populationVar, !!comp) %>%
        dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
        dplyr::ungroup() %>%
        tidyr::complete(!!populationVar,!!comp,fill=list(N=0)) %>%
        dplyr::group_by(!!populationVar) %>%
        dplyr::mutate(binom::binom.confint(x=N,n=sum(N),method="wilson"))

      ggplot2::ggplot(compare_count, ggplot2::aes(x=!!comp, color=!!populationVar,fill=!!populationVar, y=mean, ymin=lower, ymax=upper))+
        ggplot2::geom_bar(stat="identity",alpha=0.2,width=0.8,position="dodge")+
        ggplot2::geom_errorbar(width=0.3, position = ggplot2::position_dodge(width = 0.8), colour="black")+
        ggplot2::scale_color_brewer(palette = "Dark2",name="",aesthetics = c("fill","colour"))+
        ggplot2::ylab("proportion")+
        ggplot2::xlab(readable_label(!!comp))

    }

  })

}

# generate a time series of factor proportions
plot_rolling = function(linelist, factorVar, window=15) {
  grps = linelist %>% dplyr::groups()
  factorVar = rlang::ensym(factorVar)
  rolling_genomics = linelist %>% dplyr::filter(!is.na(!!factorVar)) %>% dplyr::ungroup() %>%
    dplyr::mutate(!!factorVar := forcats::fct_drop(!!factorVar)) %>%
    dplyr::group_by(!!!grps, !!factorVar, admission.date) %>% dplyr::summarise(count = dplyr::n(),.groups = "drop") %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(!!!grps),!!factorVar, admission.date = dates, fill=list(count=0)) %>%
    dplyr::group_by(!!!grps, !!factorVar) %>%
    dplyr::arrange(admission.date) %>%
    dplyr::mutate(roll.count = stats::filter(count,rep(1,window)/window)) %>%
    dplyr::group_by(!!!grps, admission.date) %>% dplyr::mutate(roll.proportion = roll.count/sum(roll.count))

  p = ggplot2::ggplot(rolling_genomics, ggplot2::aes(x=admission.date, y=roll.proportion, fill=!!factorVar,colour=!!factorVar))+ggplot2::geom_area(alpha=0.2)+
    ggplot2::ylab("proportion")+
    ggplot2::xlab("admission date")+
    ggplot2::scale_x_date(date_breaks = "2 weeks", limits = range(dates))
  if (length(grps)>0) {
    p=p+ggplot2::facet_wrap(grps)
  }
  p
}

# binomial outcome plot
plot_outcome = function(linelist, outcomeExpr, label, colour="genomic.variant_inferred", stratifyBy = "vaccination.vaccination") {
  outcomeExpr = rlang::enexpr(outcomeExpr)
  stratifyBy = rlang::ensym(stratifyBy)
  colour = rlang::ensym(colour)
  avoncap_variant_count = linelist %>%
    dplyr::group_by(!!colour, !!stratifyBy) %>%
    dplyr::summarise(binom::binom.confint(x=sum(stats::na.omit(!!outcomeExpr)),n=dplyr::n(),method="wilson"),.groups = "drop")

  ggplot2::ggplot(avoncap_variant_count, ggplot2::aes(colour=!!colour, x=!!stratifyBy, y=mean*100, ymin=lower*100, ymax=upper*100))+
    ggplot2::geom_point(position=ggplot2::position_dodge(width=0.5))+
    ggplot2::geom_errorbar(width=0.3,position=ggplot2::position_dodge(width=0.5))+
    ggplot2::ylab(label)+ggplot2::xlab(NULL)+ggplot2::scale_color_brewer(palette = "Dark2",name="")
}

plot_rolling_quantiles = function(data, decileVar, orderVar, quantiles = c(0.025,0.25,0.5,0.75,0.975), colours=c("red","blue"), linetypes=c("solid","solid"), window=15) {

  quantiles = unique(c(quantiles,0.5))
  decileVar = rlang::ensym(decileVar)
  orderVar = rlang::ensym(orderVar)
  grps = data %>% dplyr::groups()

  summaryLag = data %>% dplyr::arrange(!!orderVar) %>%
    dplyr::mutate(rollingQuant = slider::slide_index(!!decileVar, !!orderVar, .before=window %/% 2,.after=window %/% 2,.complete = TRUE,.f = ~ tibble::enframe(stats::quantile(.x, seq(0.1,0.9,length.out = 5), na.rm=TRUE)))) %>%
    dplyr::select(!!!grps, !!orderVar, rollingQuant) %>%
    tidyr::unnest(rollingQuant) %>%
    dplyr::distinct()

  ggplot2::ggplot(data = data, mapping=(ggplot2::aes(x=!!orderVar, y=!!decileVar)))+
    ggplot2::geom_line(data = summaryLag %>% dplyr::filter(name != "50%"), mapping=ggplot2::aes(x=!!orderVar,y=value, group=name), linetype=linetypes[2], colour=colours[2] )+
    ggplot2::geom_line(data = summaryLag %>% dplyr::filter(name == "50%"), mapping=ggplot2::aes(x=!!orderVar,y=value, group=name), linetype=linetypes[1], colour=colours[1] )

}

# plotRollingProportion = function(data, binomialExpr, orderVar = "date", colours=c("red","blue"), window=14) {
#   binomialExpr = rlang::enexpr(binomialExpr)
#   orderVar = rlang::ensym(orderVar)
#   grps = data %>% dplyr::groups()
#
#   overallLag = data %>% dplyr::summarise(p = sum(!!binomialExpr,na.rm = TRUE)/length(stats::na.omit(!!binomialExpr)))
#
#   summaryLag = data %>% dplyr::arrange(!!orderVar) %>%
#     dplyr::mutate(
#       x = slider::slide_index_dbl(!!binomialExpr, !!orderVar, .before=window,.after=window,.f = ~ sum(.x,na.rm = TRUE)),
#       n = slider::slide_index_dbl(!!binomialExpr, !!orderVar, .before=window,.after=window,.f = ~ length(stats::na.omit(.x))),
#     ) %>%
#     dplyr::select(!!!grps, !!orderVar, x, n) %>%
#     dplyr::distinct()
#
#   # browser()
#
#   summaryLag = summaryLag %>%
#     dplyr::filter(!is.na(x) & !is.na(n)) %>%
#     dplyr::mutate(binom::binom.confint(x,n,methods="wilson")) %>%
#     dplyr::rename(lower.0.025 = lower,upper.0.975 = upper) %>% dplyr::select(-mean) %>%
#     dplyr::mutate(binom::binom.confint(x,n,methods="wilson",conf.level=0.5)) %>%
#     dplyr::rename(lower.0.25 = lower,upper.0.75 = upper)
#
#   list(
#     ggplot2::geom_line(data = summaryLag, mapping=ggplot2::aes(x=!!orderVar,y=lower.0.025), linetype="dotted", colour=colours[1] ),
#     ggplot2::geom_line(data = summaryLag, mapping=ggplot2::aes(x=!!orderVar,y=upper.0.975), linetype="dotted", colour=colours[1] ),
#     ggplot2::geom_line(data = summaryLag, mapping=ggplot2::aes(x=!!orderVar,y=lower.0.25), linetype="dashed", colour=colours[1] ),
#     ggplot2::geom_line(data = summaryLag, mapping=ggplot2::aes(x=!!orderVar,y=upper.0.75), linetype="dashed", colour=colours[1] ),
#     ggplot2::geom_line(data = summaryLag, mapping=ggplot2::aes(x=!!orderVar,y=mean), linetype="solid", colour=colours[1] ),
#     ggplot2::geom_hline(data = overallLag, mapping = ggplot2::aes(yintercept = p), colour=colours[2])
#   )
# }


# plotRollingDensity = function(data, observationVar, n=50, orderVar = "date", colours=c("red","blue"), window=14) {
#   observationVar = rlang::ensym(observationVar)
#   orderVar = rlang::ensym(orderVar)
#   grps = data %>% dplyr::groups()
#   obs = data %>% dplyr::pull(!!observationVar)
#   breaks = seq(min(obs,na.rm = TRUE),max(obs,na.rm = TRUE),length.out = n+1)
#   miBreak = stats::na.omit(breaks+dplyr::lag(breaks))/2
#   summaryLag = data %>% dplyr::select(!!!grps,!!orderVar,!!observationVar) %>% tidyr::nest(observations = !!observationVar) %>% dplyr::arrange(!!orderVar)
#   summaryLag = summaryLag %>% dplyr::mutate(bins = slider::slide(observations, .before=14, .after=14, .f = ~dplyr::bind_rows(.x) %>%
#                                                             dplyr::mutate(bin = cut(!!observationVar,breaks = breaks,labels = miBreak)) %>%
#                                                             dplyr::group_by(bin) %>%
#                                                             dplyr::summarise(count=dplyr::n()) %>%
#                                                             dplyr::mutate(prop = count/sum(count)) %>%
#                                                             dplyr::ungroup() %>%
#                                                             tidyr::complete(bin, fill=list(count=0,prop=0)) ))
#   summaryLag = summaryLag %>% dplyr::select(!!!grps,!!orderVar,bins) %>% tidyr::unnest(bins)
#
#   list(
#     ggplot2::geom_tile(data = summaryLag,  mapping=ggplot2::aes(x=!!orderVar, y=as.numeric(as.character(bin)), fill=prop))
#   )
# }

## Odds Ratios / VE ----

# tidy_linelist_to_count = function(groupedDf, dateVar=date, ...) {
#   dots = rlang::list2(...)
#   grps = groupedDf %>% dplyr::groups()
#   dateVar = rlang::ensym(dateVar)
#   out = groupedDf %>% dplyr::group_by(!!!grps) %>% dplyr::group_modify(function(d,g,..) {
#     minMax = range(d %>% dplyr::pull(!!dateVar), finite = TRUE)
#     return(tibble::tibble(!!dateVar := seq(minMax[[1]], minMax[[2]])))
#   })
#   if (length(dots)==0) dots = list(n ~ TRUE)
#   summaries = dplyr::map(dots, ~ sprintf("%s = sum(%s)", as.character(rlang::f_lhs(.x)), as.character(rlang::f_rhs(.x))) %>% as.expression())
#   tmp = groupedDf %>% dplyr::group_by(!!!grps, !!dateVar) %>% dplyr::summarise(!!!summaries)
#
# }

linelist_to_ts = function(df, dateVar= "date", ...) {

  dots = rlang::list2(...)
  dateVar = rlang::ensym(dateVar)

  if (dplyr::is_grouped_df(df)) return(df %>% dplyr::group_modify(function(d,g,...) { linelist_to_ts(d, !!dateVar, !!!dots)  }))

  minMax = range(df %>% dplyr::pull(!!dateVar), finite = TRUE)
  dateRange = seq(from = minMax[[1]], to = minMax[[2]],by = 1)

  newCols = unlist(dplyr::map(dots, ~ rlang::f_lhs(.x)))

  summaries = dplyr::map(dots, ~ rlang::expr(sum(!!(rlang::f_rhs(.x)),na.rm=TRUE)))
  names(summaries) = sapply(newCols,as_label)

  newColFill = rep(0,length(newCols))
  names(newColFill) = sapply(newCols,as_label)
  newColFill = as.list(newColFill)
  newColFill$n = 0

  tmp = df %>% dplyr::group_by(!!dateVar) %>% dplyr::summarise(!!!summaries, n=dplyr::n(),.groups = "drop") %>% dplyr::rename(.d := !!dateVar)
  tmp = tmp %>% dplyr::ungroup() %>% tidyr::complete(.d = dateRange, fill=newColFill) %>% dplyr::arrange(.d) %>% dplyr::rename(!!dateVar := .d)

  return(tmp)
}

# tmp = avoncap_original %>%
#   linelist_to_ts(admission.date,
#     delta_vacc ~ genomic.variant == "Delta" & vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received,
#     delta_novacc ~ genomic.variant == "Delta" & vaccination.covid_vaccination != v$vaccination.covid_vaccination$Received,
#     omicron_vacc ~ genomic.variant == "Omicron" & vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received,
#     omicron_novacc ~ genomic.variant == "Omicron" & vaccination.covid_vaccination != v$vaccination.covid_vaccination$Received,
#   ) %>%
#   dplyr::mutate(across(.cols=-admission.date,.fns = function(x) slider::slide_dbl(x, sum, .before=56),.names = "roll_{col}")) %>%
#   dplyr::mutate(odds_ratio_ve(vaccinatedCase = roll_delta_vacc,unvaccinatedCase = roll_delta_novacc,vaccinatedControl = roll_omicron_vacc,unvaccinatedControl = roll_omicron_novacc))

odds_ratio_ve = function(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl, p=c(0.025,0.975)) {

  oddsR = case_when(
    vaccinatedCase == 0 | unvaccinatedCase == 0 | unvaccinatedControl == 0 | vaccinatedControl == 0 ~ NA_real_,
    TRUE ~ (vaccinatedCase/vaccinatedControl) / (unvaccinatedCase/unvaccinatedControl)
  )
  logOdds = log(oddsR)
  logOddsSD = sqrt(1/vaccinatedCase+1/unvaccinatedCase+1/vaccinatedControl+1/unvaccinatedControl)
  oddsQ = purrr::map(p, ~ exp(stats::qnorm(.x,logOdds, logOddsSD)))
  names(oddsQ) = paste0("OR.q.",p)
  veQ = purrr::map(rev(p), ~ 1 - exp(stats::qnorm(.x,logOdds, logOddsSD)))
  names(veQ) = paste0("VE.OR.q.",p)
  cbind(tibble::tibble(OR = oddsR),as.data.frame(oddsQ),tibble::tibble(VE.OR = 1-oddsR),as.data.frame(veQ))

}

relative_risk_ve = function(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl, p=c(0.025,0.975)) {
  RR = case_when(
    vaccinatedCase == 0 | unvaccinatedCase == 0 | unvaccinatedControl == 0 | vaccinatedControl == 0 ~ NA_real_,
    TRUE ~ (vaccinatedCase/(vaccinatedCase+vaccinatedControl)) / (unvaccinatedCase/(unvaccinatedCase+unvaccinatedControl))
  )
  logRR = log(RR)
  logRRSD = sqrt(1/vaccinatedCase+1/unvaccinatedCase+1/vaccinatedControl+1/unvaccinatedControl)
  oddsQ = purrr::map(p, ~ exp(stats::qnorm(.x,logRR, logRRSD)))
  names(oddsQ) = paste0("RR.q.",p)
  veQ = purrr::map(rev(p), ~ 1 - exp(stats::qnorm(.x,logRR, logRRSD)))
  names(veQ) = paste0("VE.RR.q.",p)
  cbind(tibble::tibble(RR = RR),as.data.frame(oddsQ),tibble::tibble(VE.RR = 1-RR),as.data.frame(veQ))
}


# test=tibble::tribble(
#   ~vaccinatedCase, ~unvaccinatedCase, ~vaccinatedControl, ~unvaccinatedControl, ~VE.test,
#   5,29,43,11,0.956,
#   20,43,39,7,0.917,
#   15,13,20,22,-0.269
# )
#
# test %>% dplyr::mutate(odds_ratio_ve(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl))
