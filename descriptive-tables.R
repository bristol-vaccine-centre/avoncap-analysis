
# TODO refactor to have more sensible names
# populationVar could be "dependent"
# comparisonVars could be "explanatory"
# TODO consider whether this could be substituted for functions in finalfit package
# TODO a tidier version of this

## Demographic tables ----

# iris %>% filter(Species!="setosa") %>% compare_population(populationVar = Species, comparisonVars = ggrrr::as_vars(-Species))

tidy_ks = function(df, formula) {
  populationVar = rlang::f_rhs(formula)
  comp = rlang::f_lhs(formula)
  tmp = df %>% group_modify(function(d,f,...) {
    # browser()
    if (length(unique(d %>% pull(!!populationVar))) > 2) stop("more than 2 classes in comparison.")
    if (length(unique(d %>% pull(!!populationVar))) < 2) return(tibble(p = NA, method = "2 sided ks")) # only one side present in this subgroup 
    
    side1 = d %>% group_by(!!populationVar) %>% filter(cur_group_id()==1) %>% pull(!!comp)
    side2 = d %>% group_by(!!populationVar) %>% filter(cur_group_id()==2) %>% pull(!!comp)
    
    pval = tryCatch({ks.test(side1,side2) %>% broom::tidy() %>% pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
    return(tibble(p = pval, method = "2 sided ks"))
  })
  # if (class(populationVar)=="call") comp = rlang::call_args(comp)
  return(tmp)
}

tidy_wilcoxon = function(df, formula) {
  populationVar = rlang::f_rhs(formula)
  comp = rlang::f_lhs(formula)
  tmp = df %>% group_modify(function(d,f,...) {
    # browser()
    if (length(unique(d %>% pull(!!populationVar))) > 2) stop("more than 2 classes in comparison.")
    if (length(unique(d %>% pull(!!populationVar))) < 2) return(tibble(p = NA, method = "2 sided wilcoxon")) # only one side present in this subgroup 
    
    side1 = d %>% group_by(!!populationVar) %>% filter(cur_group_id()==1) %>% pull(!!comp)
    side2 = d %>% group_by(!!populationVar) %>% filter(cur_group_id()==2) %>% pull(!!comp)
    
    pval = tryCatch({wilcox.test(side1,side2) %>% broom::tidy() %>% pull(p.value)}, error=function(e) NA) # not able to perform ks test - e.g. all zeros
    return(tibble(p = pval, method = "2 sided wilcoxon"))
  })
  # if (class(populationVar)=="call") comp = rlang::call_args(comp)
  return(tmp)
}

compare_population = function(
  avoncap_variants, 
  populationVar = "genomic.variant", 
  comparisonVars = vars(demog.age,demog.gender,demog.ethnicity),
  na.rm=FALSE,
  p.value=TRUE
) {
  populationVar = ensym(populationVar)
  avoncap_variants = avoncap_variants %>% ungroup()
  
  missingness = avoncap_variants %>% filter(is.na(!!populationVar)) %>% nrow()
  if(missingness > 0) warning("excluding ",missingness," records with missing dependent variable")
  avoncap_variants = avoncap_variants %>% filter(!is.na(!!populationVar)) %>% mutate(across(where(is.factor), forcats::fct_drop))
  
  comparisonList = lapply(comparisonVars, function(comp) {
    
    variableName = readable_label(!!comp)
    variableLabel = as_label(comp)
    if (avoncap_variants %>% pull(!!comp) %>% is.numeric()) {
      
      compare_count = avoncap_variants %>% 
        group_by(side = !!populationVar) %>%
        summarise(
          N=sum(!(is.na(!!comp))),
          mean.value = mean(!!comp,na.rm = TRUE),
          sd.value = sd(!!comp,na.rm = TRUE),
          median = quantile(!!comp,0.5,na.rm = TRUE),
          lqr = quantile(!!comp,0.25,na.rm = TRUE),
          uqr = quantile(!!comp,0.75,na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        mutate(variable = variableName, variable.original = variableLabel) %>% 
        select(side,variable,variable.original,N,mean.value,sd.value,median,lqr,uqr)
      
      # How many sides does the comparison have.
      sides = compare_count %>% pull(side) %>% unique() %>% length()
      
      # test for skewness possible but upper limit is a function of size:
      # this simulates:0.95 CI for different sample sizes
      # tmp = tibble(skew95 = sapply(seq(1,4,length.out = 20),function(f) sapply(1:100,function(e) e1071::skewness(rnorm(10^f))) %>% abs() %>% quantile(0.95)), size = 10^seq(1,4,length.out = 20))
      # skew95 and size have a log-log relationship
      # ggplot(tmp, aes(x=size,y=skew95))+geom_point()+geom_smooth()+scale_x_log10()+scale_y_log10()
      # which can be fitted:
      # lm(formula = log(skew95) ~ log(size), tmp) giving the following relationship for the 95% CI of the absolute value of skewness in a normal:
      skew95 = function(size) {exp(1.23740 -0.45564 * log(size))}
      
      # Determine if the continuous variable is normally distributed
      # This uses shapiro wilkes test, or for larger groups (>5000) a test to see if the skew is > 95% of expected for a normal distribution.
      parametric = tryCatch({
          # cannot reject null hypothesis that it is normally distributed at 0.05
          # will throw an error if size is gt 5000.
          shap = (avoncap_variants %>% group_by(side = !!populationVar) %>% rstatix::shapiro_test(!!comp) %>% pull(p) %>% min())
          message(as_label(comp),": lowest p-value for shapiro wilks test: ",shap)
          shap > 0.05
          # Shapiro test will fail for larger N
        }, error = function(e) {
          # browser()
          # abs(skew) is larger than expected for a normal at p = 0.05
          tryCatch({
            maxSkew = avoncap_variants %>% group_by(side = !!populationVar) %>% summarise(skew = abs(e1071::skewness(!!comp, na.rm=TRUE,type=2)), size = n(),.groups = "drop") %>% 
              mutate(skew95 = skew95(size)) %>% filter(skew/skew95 == suppressWarnings(max(skew/skew95)))
            message(as_label(comp),": max skew value is ",maxSkew$skew," versus expected 95% quantile ",maxSkew$skew95," on ",maxSkew$size," samples.")
            maxSkew$skew < maxSkew$skew95
          }, error = function(e) FALSE)
      })
      
      if(length(parametric)==0) parametric = FALSE
      
      if (parametric) {
        compare_count = compare_count %>% mutate(
          value = sprintf("%1.3g \u00B1 %1.3g",mean.value,sd.value),
          alt.value = sprintf("%1.3g \u00B1 %1.3g (%1.0f)",mean.value,sd.value,N),
          group = factor("(mean \u00B1 SD)")
        )
      } else {
        compare_count = compare_count %>% mutate(
          value = sprintf("%1.3g [%1.3g \u2013 %1.3g]",median,lqr,uqr),
          alt.value = sprintf("%1.3g [%1.3g \u2013 %1.3g] (%1.0f)",median,lqr,uqr,N),
          group = factor("(median [IQR])")
        )
      }
      
      # TODO: TEST THIS: what about p-value when more than 2 comparison classes
      # https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/#anova
      # kruskal.test or anova depending on normality assumption
      if (p.value & sides > 1) {
        
        formula = as.formula(paste0(as_label(comp)," ~ ",as_label(populationVar)))
        
        if (!parametric) {
          # this is a non parametric problem
          
          if (sides == 2) {
            # we can use ks test to for 2 sides non-parametric
            tmp = avoncap_variants %>% pull(!!comp)
            tiesFrac = 1-length(unique(tmp))/length(tmp)
            # However if there are lots of ties (and we have enabled it a wilcoxon is better)
            if (getOption("use.wilcoxon",TRUE) & tiesFrac > 0.75) {
              pval = avoncap_variants %>% ungroup() %>% tidy_wilcoxon(formula) %>% pull(p)
              pmethod = "2-sided wilcoxon"
            } else {
              pval = avoncap_variants %>% ungroup() %>% tidy_ks(formula) %>% pull(p)
              pmethod = "2-sided ks"
            }
          } else {
            # more than 2 sides
            # we can use kruskal-wallis test to detect if all samples come from same distribution
            pval = avoncap_variants %>% rstatix::kruskal_test(formula) %>% pull(p)
            pmethod = "kruskal"
          }
        } else {
          # parametric
          if (sides == 2) {
            
            pval = avoncap_variants %>% rstatix::t_test(formula) %>% pull(p)
            pmethod = "t-test"
          } else {
            
            pval = avoncap_variants %>% rstatix::anova_test(formula) %>% pull(p)
            pmethod = "anova"
          }
        }  
        
        compare_count = compare_count %>% mutate(
          p.value = if(is.na(pval)) "\u2014" else scales::pvalue(pval), 
          p.method=pmethod,
          p.excluding_missing = pval
        )
        
        #browser()
        
        
      }
      
      return(compare_count %>% select(-c(mean.value,sd.value,median,lqr,uqr)))
      
    } else if (avoncap_variants %>% pull(!!comp) %>% is.ordered() | avoncap_variants %>% pull(!!comp) %>% is.logical() | avoncap_variants %>% pull(!!comp) %>% is.factor()) {
      
      formula = as.formula(paste0(as_label(comp)," ~ ",as_label(populationVar)))
      tmp = avoncap_variants
      
      if (avoncap_variants %>% pull(!!comp) %>% is.logical()) {
        tmp = tmp %>% mutate(!!comp := !!comp %>% ordered(labels=c("false","true")))
      }
      
      if (na.rm) {
        tmp = tmp %>% filter(!is.na(!!comp))
      } else {
        tmp = tmp %>% mutate(!!comp := forcats::fct_explicit_na(!!comp, "<missing>"))
      }
      
      tmp = tmp %>% mutate(
        !!populationVar := forcats::fct_drop(!!populationVar), 
        !!comp := forcats::fct_drop(!!comp),
        side = !!populationVar, 
        group = !!comp
      )
      
      compare_count = tmp %>%
        group_by(side, group) %>%
        summarise(N = n(),.groups = "drop") %>%
        ungroup() %>%
        tidyr::complete(side,group,fill=list(N=0)) %>%
        group_by(side) %>%
        mutate(binom::binom.confint(x=N,n=sum(N),method="wilson")) %>%
        mutate(
          # value = sprintf("%1.1f%% [%1.1f%%\u00A0\u2013\u00A0%1.1f%%]",mean*100,lower*100,upper*100),
          value = sprintf("%1.1f%% [%1.1f%%\u2013%1.1f%%]",mean*100,lower*100,upper*100),
          alt.value = sprintf("%1.1f%% (%1.0f/%1.0f)",mean*100,x,n),
          variable = variableName,
          variable.original = variableLabel
        ) %>%
        select(side,variable,variable.original,group,N,value,alt.value) %>%
        ungroup()
      # %>%
      # tidyr::complete(side,variable,group,fill=list(N=0, value="\u2014"))
      
      sides = compare_count %>% pull(side) %>% unique() %>% length()
      
      if (p.value & sides > 1) {
        
        # TODO: https://www.sheffield.ac.uk/polopoly_fs/1.885177!/file/93_ChiSquare.pdf
        # check assumptions 
        # or use fishers exact test
        
        # chi.data = matrix(
        #   compare_count %>% arrange(side,group) %>% pull(N),
        #   ncol = sides)
        
        chi.data = with(tmp, table(side,group))
        
        # pval = suppressWarnings(chisq.test(chi.data) %>% broom::tidy() %>% pull(p.value))
        pval = tryCatch({
          suppressWarnings(fisher.test(chi.data, simulate.p.value=TRUE,B=1e5) %>% broom::tidy() %>% pull(p.value))
        },error= function(e) NA_real_)
        pmethod="fisher"
        
        compare_count = compare_count %>% mutate(
          p.value = if(is.na(pval)) "\u2014" else scales::pvalue(pval), 
          p.method=pmethod,
          p.including_missing = pval
        )
        
        # what would the association have been without missing data categories
        if (!na.rm) {
          chi.data2 = with(tmp %>% filter(group != "<missing>") %>% mutate(group= group %>% forcats::fct_drop()), table(side,group))
          
          # pval = suppressWarnings(chisq.test(chi.data) %>% broom::tidy() %>% pull(p.value))
          pmissing = tryCatch({
            suppressWarnings(fisher.test(chi.data2,simulate.p.value=TRUE,B=1e5) %>% broom::tidy() %>% pull(p.value))
          },error= function(e) NA_real_)
          
          compare_count = compare_count %>% 
            mutate(p.excluding_missing = pmissing)
          
          compare_count = compare_count %>%
            mutate(
              p.value = ifelse(is.na(p.excluding_missing) | is.na(p.including_missing), p.value,
                sprintf("%s (%s)",
                  scales::pvalue(p.excluding_missing),
                  scales::pvalue(p.including_missing)
                ))
            )
        }
        
      }
      
      return(compare_count)
    } else {
      message("skipping comparison: ",as_label(comp)," value is a ",class(avoncap_variants %>% pull(!!comp)))
      return(tibble(side=character(),variable=character(),variable.original=character(),group=character(),N=integer(),value=character(),alt.value=character()))
    }
  })
  
  sideList = avoncap_variants %>% pull(!!populationVar) %>% forcats::fct_drop() %>% levels()
  # TODO: figure out wher this is going wrong...
  # levelList = comparisonList %>% sapply(function(.d) try(.d %>% pull(group) %>% levels())) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  # groupLevels = unlist(unname((sapply(levelList, function(.l) .l[.l != "<missing>"]))))
  # groupLevels = c("(mean \u00B1 SD)",levelList)
  
  comparisonLabels = readable_label_mapping(comparisonVars) %>% unique(fromLast=TRUE)
  # browser()
  out = ggrrr::bind_rows_with_factors(!!!comparisonList)
  out = out %>% mutate(
      group = suppressWarnings(forcats::fct_relevel(group, "<missing>", after = Inf)),
      side = ordered(side, levels = sideList),
      variable = ordered(variable, levels=unname(comparisonLabels))
    ) %>% 
    arrange(side,variable,group)
    
  # Combine list of sub-analyses into one data-frame  
  # out = comparisonList %>% lapply(function(.d) {
  #   # convert factors to text
  #   #if(is.null(.d)) return(tibble())
  #   .d %>% mutate(
  #     side = as.character(side),
  #     group = as.character(group),
  #     variable = as.character(variable)
  #   )
  # }) %>% 
  #   # combine dataframes
  #   bind_rows() %>%
  #   # convert text back to factors back
  #   mutate(
  #     side = ordered(side, levels = sideList),
  #     group = ordered(group, levels = groupLevels),
  #     variable = ordered(variable, levels=unname(comparisonLabels))
  #   )
  # 
  # 
  
  return(out)
  
}

population_comparison_table = function(comparison, show_method=TRUE, compact=FALSE ) {
  grps = comparison %>% groups()
  show_p = "p.value" %in% colnames(comparison)
  if (show_p) {
    if (show_method) {
      tmp2 = comparison %>% 
        select(!tidyselect::any_of(c("p.including_missing","p.excluding_missing","variable.original"))) %>%
        rename(Characteristic = variable,Group = group, `P-value`=p.value,method=p.method)
      if (compact) {
        tmp2 = tmp2 %>% select(-N,-value) %>% rename(value = alt.value)
      } else {
        tmp2 = tmp2 %>% select(-alt.value)
      }
      tmp2 = tmp2 %>% hux_tidy(rowGroupVars = vars(Characteristic,!!!grps,`P-value`,method,Group),colGroupVars = vars(side)) %>% relocate(`P-value`,method,.after=last_col())
      
    } else {
      tmp2 = comparison %>% 
        select(!tidyselect::any_of(c("p.including_missing","p.excluding_missing","variable.original","p.method"))) %>%
        rename(Characteristic = variable,Group = group, `P-value`=p.value)
      if (compact) {
        tmp2 = tmp2 %>% select(-N,-value) %>% rename(value = alt.value)
      } else {
        tmp2 = tmp2 %>% select(-alt.value)
      }
      tmp2 = tmp2 %>% hux_tidy(rowGroupVars = vars(Characteristic,!!!grps,`P-value`,Group),colGroupVars = vars(side)) %>% relocate(`P-value`,.after=last_col())
    }
  } else {
    tmp2 = comparison %>% 
      select(!tidyselect::any_of(c("p.including_missing","p.excluding_missing","variable.original","p.method","p.value"))) %>%
      rename(Characteristic = variable,Group = group)
    if (compact) {
      tmp2 = tmp2 %>% select(-N,-value) %>% rename(value = alt.value)
    } else {
      tmp2 = tmp2 %>% select(-alt.value)
    }
    tmp2 = tmp2 %>% hux_tidy(rowGroupVars = vars(Characteristic,!!!grps, Group),colGroupVars = vars(side))
  }
  tmp2
}

## Basic stats ----

describe_population = function(
  avoncap_variants, 
  comparisonVars = vars(demog.age,demog.gender,demog.ethnicity),
  na.rm=FALSE, CI=TRUE
) {
  avoncap_variants = avoncap_variants %>% ungroup()
  
  comparisonList = lapply(comparisonVars, function(comp) {
    
    variableName = readable_label(!!comp)
    variableLabel = as_label(comp)
    if (avoncap_variants %>% pull(!!comp) %>% is.numeric()) {
      
      compare_count = avoncap_variants %>% 
        summarise(
          N=sum(!(is.na(!!comp))),
          mean.value = mean(!!comp,na.rm = TRUE),
          sd.value = sd(!!comp,na.rm = TRUE),
          value = sprintf("%1.3g \u00B1 %1.3g",mean.value,sd.value),
          group=factor("(mean \u00B1 SD)"),
          .groups = "drop"
        ) %>% 
        mutate(variable = variableName, variable.original = variableLabel) %>% 
        select(variable,variable.original,group,N,value)
      
      return(compare_count)
      
    } else if (avoncap_variants %>% pull(!!comp) %>% is.ordered() | avoncap_variants %>% pull(!!comp) %>% is.logical() | avoncap_variants %>% pull(!!comp) %>% is.factor()) {
      
      tmp = avoncap_variants
      
      if (avoncap_variants %>% pull(!!comp) %>% is.logical()) {
        tmp = tmp %>% mutate(!!comp := !!comp %>% ordered(labels=c("false","true")))
      }
      
      if (na.rm) {
        tmp = tmp %>% filter(!is.na(!!comp))
      } else {
        tmp = tmp %>% mutate(!!comp := forcats::fct_explicit_na(!!comp, "<missing>"))
      }
      
      tmp = tmp %>% mutate(
        !!comp := forcats::fct_drop(!!comp),
        group = !!comp
      )
      
      compare_count = tmp %>%
        group_by(group) %>%
        summarise(N = n(),.groups = "drop") %>%
        ungroup() %>%
        tidyr::complete(group,fill=list(N=0)) %>%
        ungroup() %>%
        mutate(binom::binom.confint(x=N,n=sum(N),method="wilson")) 
      
      if (CI) {
        compare_count = compare_count %>%
          mutate(
            value = sprintf("%1.1f%% [%1.1f%%\u2013%1.1f%%]",mean*100,lower*100,upper*100),
          )
      } else {
        compare_count = compare_count %>%
          mutate(
            value = sprintf("%1.1f%%",mean*100)
          )
      }
      
      compare_count = compare_count %>% mutate(
          variable = variableName,
          variable.original = variableLabel
        ) %>%
        select(variable,variable.original,group,N,value) %>%
        ungroup()
      # %>%
      # tidyr::complete(side,variable,group,fill=list(N=0, value="\u2014"))
      
      return(compare_count)
    } else {
      message("skipping comparison: ",as_label(comp)," value is a ",class(avoncap_variants %>% pull(!!comp)))
      return(tibble(side=character(),variable=character(),variable.original=character(),group=character(),N=integer(),value=character()))
    }
  })
  
  # levelList = comparisonList %>% sapply(function(.d) try(.d %>% pull(group) %>% levels())) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  # # groupLevels = unlist(unname((sapply(levelList, function(.l) .l[.l != "<missing>"]))))
  # groupLevels = c("(mean \u00B1 SD)",levelList)
  # 
  # comparisonLabels = readable_label_mapping(comparisonVars) %>% unique(fromLast=TRUE)
  # 
  # # Combine list of sub-analyses into one data-frame  
  # out = comparisonList %>% lapply(function(.d) {
  #   # convert factors to text
  #   #if(is.null(.d)) return(tibble())
  #   .d %>% mutate(
  #     group = as.character(group),
  #     variable = as.character(variable)
  #   )
  # }) %>% 
  #   # combine dataframes
  #   bind_rows() %>%
  #   # convert text back to factors back
  #   mutate(
  #     group = factor(group, levels = groupLevels),
  #     variable = factor(variable, levels=unname(comparisonLabels))
  #   )
  
  # sideList = avoncap_variants %>% pull(!!populationVar) %>% forcats::fct_drop() %>% levels()
  # TODO: figure out wher this is going wrong...
  # levelList = comparisonList %>% sapply(function(.d) try(.d %>% pull(group) %>% levels())) %>% unlist() %>% unname() %>% unique(fromLast=TRUE)
  # groupLevels = unlist(unname((sapply(levelList, function(.l) .l[.l != "<missing>"]))))
  # groupLevels = c("(mean \u00B1 SD)",levelList)
  
  comparisonLabels = readable_label_mapping(comparisonVars) %>% unique(fromLast=TRUE)
  out = ggrrr::bind_rows_with_factors(!!!comparisonList)
  out = out %>% mutate(
    group = forcats::fct_relevel(group, "<missing>", after = Inf),
    variable = ordered(variable, levels=unname(comparisonLabels))
  ) %>% 
    arrange(variable,group)
  
  
  return(out)
  
}


## Missing values ----

compare_missingness = function(
  avoncap_variants, 
  populationVar = "genomic.variant", 
  comparisonVars = vars(demog.age,demog.gender,demog.ethnicity),
  na.rm=FALSE
) {
  populationVar = ensym(populationVar)
  tmp = avoncap_variants
  for (comp in comparisonVars) {
    tmp = tmp %>% mutate(!!comp := ifelse(is.na(!!comp),"missing","not missing") %>% ordered(c("not missing","missing")))
  }
  out = compare_population(tmp,populationVar=!!populationVar,comparisonVars = comparisonVars,na.rm = TRUE, p.value=TRUE)
  out %>% complete(side,nesting(variable,variable.original),group, fill=list(N=0,p.value="\u2014")) %>% 
    select(-c(value, p.value,p.method,alt.value)) %>%
    filter(group %in% c("missing","not missing")) %>%
    pivot_wider(values_from = N, names_from= group) %>%
    arrange(variable) %>%
    rename(p.missing_at_random=p.including_missing) %>%
    return()
}

missing_comparison_table = function(comparison) {
  grps = comparison %>% groups()
  tmp2 = comparison %>%
    select(!tidyselect::any_of(c("p.including_missing","p.excluding_missing","variable.original"))) %>%
    mutate(p.missing = missing/(missing+`not missing`)) %>%
    group_by(!!!grps, variable) %>%
    mutate(p.missing_overall = sum(missing)/sum(missing+`not missing`)) %>%
    mutate(
      N = missing+`not missing`,
      p.value = ifelse(is.na(p.missing_at_random),"\u2014",scales::pvalue(p.missing_at_random)),
      `% missing`=sprintf("%1.1f%%",p.missing*100),
      `% missing (total)`=sprintf("%1.1f%%",p.missing_overall*100)
    ) %>%
    select(-c(p.missing,p.missing_overall, p.missing_at_random,`not missing`,N)) %>%
    #relocate(`% missing (total)`,p.value,.after=last_col()) %>%
    hux_tidy(rowGroupVars = vars(!!!grps,variable,`% missing (total)`,p.value),colGroupVars = vars(side))
  # tmp2
  tmp2 %>% relocate(`% missing (total)`,p.value,.after=last_col())
}

## Plotting functions ----

plot_population = function(
  avoncap_variants, 
  populationVar = "genomic.variant", 
  comparisonVars = vars(demog.age,demog.gender,demog.ethnicity),
  na.rm=FALSE
) {
  populationVar = ensym(populationVar)
  avoncap_variants = avoncap_variants %>% ungroup()
  
  lapply(comparisonVars, function(comp) {
    
    if (avoncap_variants %>% pull(!!comp) %>% is.numeric()) {
      
      ggplot(avoncap_variants, aes(x=!!comp, color=!!populationVar,fill=!!populationVar))+
        geom_density(alpha=0.2)+
        scale_color_brewer(palette = "Dark2",name="",aesthetics = c("fill","colour"))+
        xlab(readable_label(!!comp))
      
    } else if (avoncap_variants %>% pull(!!comp) %>% is.factor()) {
      
      if (na.rm) {
        avoncap_variants = avoncap_variants %>% filter(!is.na(!!comp))
      } else {
        avoncap_variants = avoncap_variants %>% mutate(!!comp := forcats::fct_explicit_na(!!comp, "<missing>"))
      }
      
      avoncap_variants = avoncap_variants %>% mutate(
        !!populationVar := forcats::fct_drop(!!populationVar), 
        !!comp := forcats::fct_drop(!!comp) 
      ) 
      
      compare_count = avoncap_variants %>%
        group_by(!!populationVar, !!comp) %>%
        summarise(N = n(), .groups = "drop") %>%
        ungroup() %>%
        tidyr::complete(!!populationVar,!!comp,fill=list(N=0)) %>%
        group_by(!!populationVar) %>%
        mutate(binom::binom.confint(x=N,n=sum(N),method="wilson"))
      
      ggplot(compare_count, aes(x=!!comp, color=!!populationVar,fill=!!populationVar, y=mean, ymin=lower, ymax=upper))+
        geom_bar(stat="identity",alpha=0.2,width=0.8,position="dodge")+
        geom_errorbar(width=0.3, position = position_dodge(width = 0.8), colour="black")+
        scale_color_brewer(palette = "Dark2",name="",aesthetics = c("fill","colour"))+
        ylab("proportion")+
        xlab(readable_label(!!comp))
      
    }
    
  })
  
}

# generate a time series of factor proportions
plot_rolling = function(avoncap_variants, factorVar, window=15) {
  grps = avoncap_variants %>% groups()
  factorVar = ensym(factorVar)
  rolling_genomics = avoncap_variants %>% filter(!is.na(!!factorVar)) %>% ungroup() %>% 
    mutate(!!factorVar := forcats::fct_drop(!!factorVar)) %>% 
    group_by(!!!grps, !!factorVar, admission.date) %>% summarise(count = n(),.groups = "drop") %>%
    ungroup() %>% 
    tidyr::complete(tidyr::nesting(!!!grps),!!factorVar, admission.date = dates, fill=list(count=0)) %>%
    group_by(!!!grps, !!factorVar) %>% 
    arrange(admission.date) %>% 
    mutate(roll.count = stats::filter(count,rep(1,window)/window)) %>%
    group_by(!!!grps, admission.date) %>% mutate(roll.proportion = roll.count/sum(roll.count))
  
  p = ggplot(rolling_genomics, aes(x=admission.date, y=roll.proportion, fill=!!factorVar,colour=!!factorVar))+geom_area(alpha=0.2)+
    ylab("proportion")+
    xlab("admission date")+
    scale_x_date(date_breaks = "2 weeks", limits = range(dates))
  if (length(grps)>0) {
    p=p+facet_wrap(grps)
  }
  p
}

# binomial outcome plot
plot_outcome = function(avoncap_variants, outcomeExpr, label, colour="genomic.variant_inferred", stratifyBy = "vaccination.vaccination") {
  outcomeExpr = enexpr(outcomeExpr)
  stratifyBy = ensym(stratifyBy)
  colour = ensym(colour)
  avoncap_variant_count = avoncap_variants %>%
    group_by(!!colour, !!stratifyBy) %>%
    summarise(binom::binom.confint(x=sum(na.omit(!!outcomeExpr)),n=n(),method="wilson"),.groups = "drop")
  
  ggplot(avoncap_variant_count, aes(colour=!!colour, x=!!stratifyBy, y=mean*100, ymin=lower*100, ymax=upper*100))+
    geom_point(position=position_dodge(width=0.5))+
    geom_errorbar(width=0.3,position=position_dodge(width=0.5))+
    ylab(label)+xlab(NULL)+scale_color_brewer(palette = "Dark2",name="")
} 

plot_rolling_quantiles = function(data, decileVar, orderVar, quantiles = c(0.025,0.25,0.5,0.75,0.975), colours=c("red","blue"), linetypes=c("solid","solid"), window=15) {
  
  quantiles = unique(c(quantiles,0.5))
  decileVar = ensym(decileVar)
  orderVar = ensym(orderVar)
  grps = data %>% groups()
  
  summaryLag = data %>% arrange(!!orderVar) %>% 
    mutate(rollingQuant = slider::slide_index(!!decileVar, !!orderVar, .before=window %/% 2,.after=window %/% 2,.complete = TRUE,.f = ~ enframe(quantile(.x, seq(0.1,0.9,length.out = 5), na.rm=TRUE)))) %>%
    select(!!!grps, !!orderVar, rollingQuant) %>%
    unnest(rollingQuant) %>%
    distinct()
  
  ggplot(data = data, mapping=(aes(x=!!orderVar, y=!!decileVar)))+
    geom_line(data = summaryLag %>% filter(name != "50%"), mapping=aes(x=!!orderVar,y=value, group=name), linetype=linetypes[2], colour=colours[2] )+
    geom_line(data = summaryLag %>% filter(name == "50%"), mapping=aes(x=!!orderVar,y=value, group=name), linetype=linetypes[1], colour=colours[1] )
  
}

# plotRollingProportion = function(data, binomialExpr, orderVar = "date", colours=c("red","blue"), window=14) {
#   binomialExpr = enexpr(binomialExpr)
#   orderVar = ensym(orderVar)
#   grps = data %>% groups()
#   
#   overallLag = data %>% summarise(p = sum(!!binomialExpr,na.rm = TRUE)/length(na.omit(!!binomialExpr)))
#   
#   summaryLag = data %>% arrange(!!orderVar) %>% 
#     mutate(
#       x = slider::slide_index_dbl(!!binomialExpr, !!orderVar, .before=window,.after=window,.f = ~ sum(.x,na.rm = TRUE)),
#       n = slider::slide_index_dbl(!!binomialExpr, !!orderVar, .before=window,.after=window,.f = ~ length(na.omit(.x))),
#     ) %>%
#     select(!!!grps, !!orderVar, x, n) %>%
#     distinct() 
#   
#   # browser()
#   
#   summaryLag = summaryLag %>%
#     filter(!is.na(x) & !is.na(n)) %>%
#     mutate(binom::binom.confint(x,n,methods="wilson")) %>%
#     rename(lower.0.025 = lower,upper.0.975 = upper) %>% select(-mean) %>%
#     mutate(binom::binom.confint(x,n,methods="wilson",conf.level=0.5)) %>%
#     rename(lower.0.25 = lower,upper.0.75 = upper)
#   
#   list(
#     geom_line(data = summaryLag, mapping=aes(x=!!orderVar,y=lower.0.025), linetype="dotted", colour=colours[1] ),
#     geom_line(data = summaryLag, mapping=aes(x=!!orderVar,y=upper.0.975), linetype="dotted", colour=colours[1] ),
#     geom_line(data = summaryLag, mapping=aes(x=!!orderVar,y=lower.0.25), linetype="dashed", colour=colours[1] ),
#     geom_line(data = summaryLag, mapping=aes(x=!!orderVar,y=upper.0.75), linetype="dashed", colour=colours[1] ),
#     geom_line(data = summaryLag, mapping=aes(x=!!orderVar,y=mean), linetype="solid", colour=colours[1] ),
#     geom_hline(data = overallLag, mapping = aes(yintercept = p), colour=colours[2])
#   )
# }


# plotRollingDensity = function(data, observationVar, n=50, orderVar = "date", colours=c("red","blue"), window=14) {
#   observationVar = ensym(observationVar)
#   orderVar = ensym(orderVar)
#   grps = data %>% groups()
#   obs = data %>% pull(!!observationVar)
#   breaks = seq(min(obs,na.rm = TRUE),max(obs,na.rm = TRUE),length.out = n+1)
#   miBreak = na.omit(breaks+lag(breaks))/2
#   summaryLag = data %>% select(!!!grps,!!orderVar,!!observationVar) %>% nest(observations = !!observationVar) %>% arrange(!!orderVar) 
#   summaryLag = summaryLag %>% mutate(bins = slider::slide(observations, .before=14, .after=14, .f = ~bind_rows(.x) %>% 
#                                                             mutate(bin = cut(!!observationVar,breaks = breaks,labels = miBreak)) %>% 
#                                                             group_by(bin) %>% 
#                                                             summarise(count=n()) %>% 
#                                                             mutate(prop = count/sum(count)) %>% 
#                                                             ungroup() %>% 
#                                                             tidyr::complete(bin, fill=list(count=0,prop=0)) ))
#   summaryLag = summaryLag %>% select(!!!grps,!!orderVar,bins) %>% unnest(bins)
#   
#   list(
#     geom_tile(data = summaryLag,  mapping=aes(x=!!orderVar, y=as.numeric(as.character(bin)), fill=prop))
#   )
# }

## Odds Ratios / VE ----

# tidy_linelist_to_count = function(groupedDf, dateVar=date, ...) {
#   dots = rlang::list2(...)
#   grps = groupedDf %>% groups()
#   dateVar = ensym(dateVar)
#   out = groupedDf %>% group_by(!!!grps) %>% group_modify(function(d,g,..) {
#     minMax = range(d %>% pull(!!dateVar), finite = TRUE)
#     return(tibble(!!dateVar := seq(minMax[[1]], minMax[[2]])))
#   })
#   if (length(dots)==0) dots = list(n ~ TRUE)
#   summaries = map(dots, ~ sprintf("%s = sum(%s)", as.character(rlang::f_lhs(.x)), as.character(rlang::f_rhs(.x))) %>% as.expression())
#   tmp = groupedDf %>% group_by(!!!grps, !!dateVar) %>% summarise(!!!summaries)
#   
# }

linelist_to_ts = function(df, dateVar= "date", ...) {
  
  dots = rlang::list2(...)
  dateVar = ensym(dateVar)
  
  if (is_grouped_df(df)) return(df %>% group_modify(function(d,g,...) { linelist_to_ts(d, !!dateVar, !!!dots)  }))
  
  minMax = range(df %>% pull(!!dateVar), finite = TRUE)
  dateRange = seq(from = minMax[[1]], to = minMax[[2]],by = 1)
  
  newCols = unlist(map(dots, ~ rlang::f_lhs(.x)))
  
  summaries = map(dots, ~ expr(sum(!!(rlang::f_rhs(.x)),na.rm=TRUE)))
  names(summaries) = sapply(newCols,as_label)
  
  newColFill = rep(0,length(newCols))
  names(newColFill) = sapply(newCols,as_label)
  newColFill = as.list(newColFill)
  newColFill$n = 0
  
  tmp = df %>% group_by(!!dateVar) %>% summarise(!!!summaries, n=n(),.groups = "drop") %>% rename(.d := !!dateVar)
  tmp = tmp %>% ungroup() %>% tidyr::complete(.d = dateRange, fill=newColFill) %>% arrange(.d) %>% rename(!!dateVar := .d)
  
  return(tmp)
}

# tmp = avoncap_original %>% 
#   linelist_to_ts(admission.date,
#     delta_vacc ~ genomic.variant == "Delta" & vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received,
#     delta_novacc ~ genomic.variant == "Delta" & vaccination.covid_vaccination != v$vaccination.covid_vaccination$Received,
#     omicron_vacc ~ genomic.variant == "Omicron" & vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received,
#     omicron_novacc ~ genomic.variant == "Omicron" & vaccination.covid_vaccination != v$vaccination.covid_vaccination$Received,
#   ) %>%
#   mutate(across(.cols=-admission.date,.fns = function(x) slider::slide_dbl(x, sum, .before=56),.names = "roll_{col}")) %>%
#   mutate(odds_ratio_ve(vaccinatedCase = roll_delta_vacc,unvaccinatedCase = roll_delta_novacc,vaccinatedControl = roll_omicron_vacc,unvaccinatedControl = roll_omicron_novacc))

odds_ratio_ve = function(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl, p=c(0.025,0.975)) {
  
  odds = vaccinatedCase*unvaccinatedControl / (vaccinatedControl*unvaccinatedCase)
  logOdds = log(odds)
  logOddsSD = sqrt(1/vaccinatedCase+1/unvaccinatedCase+1/vaccinatedControl+1/unvaccinatedControl)
  oddsQ = map(p, ~ exp(qnorm(.x,logOdds, logOddsSD)))
  names(oddsQ) = paste0("OR.q.",p)
  veQ = map(rev(p), ~ 1 - exp(qnorm(.x,logOdds, logOddsSD)))
  names(veQ) = paste0("VE.q.",p)
  cbind(tibble(OR = odds),as.data.frame(oddsQ),tibble(VE = 1-odds),as.data.frame(veQ))
  
}

# test=tribble(
#   ~vaccinatedCase, ~unvaccinatedCase, ~vaccinatedControl, ~unvaccinatedControl, ~VE.test,
#   5,29,43,11,0.956,
#   20,43,39,7,0.917,
#   15,13,20,22,-0.269
# )
# 
# test %>% mutate(odds_ratio_ve(vaccinatedCase, unvaccinatedCase, vaccinatedControl, unvaccinatedControl))
