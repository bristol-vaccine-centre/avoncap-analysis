# ## Bespoke plot data:
# 
# # variants dates
# variants = tribble(
#   ~variant, ~start_week, ~end_week,
#   "Wildtype", 0, 43, 
#   "Alpha", 56, 72,
#   "Delta", 75, 99,
#   "Omicron", 113, Inf
# ) %>%
#   mutate(
#     start_date = .weeks_to_date(start_week),
#     end_date = .weeks_to_date(end_week)
#   )
# 
# # time sequence during which different variantes were active
# variants_rug = variants %>% 
#   tidyr::crossing(
#     tibble(
#       date = full_seq_dates(admission_date),
#       week = .dates_to_week(date)
#     )) %>%
#   filter(start_week < week & week <= end_week)
# 
# ## Smoothing and interpolation functions
# 
# # takes a line list of patient admissions and estimates weekly rates based on 
# # a quasi-poisson model fitted to count data using local regression.
# # expects admissions to contain admission_week columns only defining the date of admission
# estimateWeeklyRate = function(admissions, ... ,nn=0.2,deg=2) {
#   admissionCounts = admissions %>% group_by(admission_week) %>% count()
#   fit = locfit::locfit(n~locfit::lp(admission_week,nn=nn,deg=deg),data = admissionCounts,family="qpoisson")
#   weeks = seq(min(admissionCounts$admission_week),max(admissionCounts$admission_week),by = 1/7)
#   tmp = preplot(fit,newdata=weeks,se.fit = TRUE,band="local")
#   t = tmp$tr
#   tibble(
#     admission_week = weeks,
#     admission_date = .weeks_to_date(weeks),
#     lower = .opt(t(qnorm(0.05,tmp$fit,tmp$se.fit))),
#     median = t(qnorm(0.5,tmp$fit,tmp$se.fit)),
#     upper = .opt(t(qnorm(0.95,tmp$fit,tmp$se.fit)))
#   )
# }
# 
# # takes a line list of patient admissions with a multinomial class label, and fits
# # a quasi-binomial model with logit link using local regression. This expects a dataframe 
# # with an admission_week column and a class column. Multinomial class is either treated as a 
# # set of 1 versus others binomials (cumulative = FALSE) or as a set of less than or equal versus more 
# # than binomials (cumulative = TRUE, which assumes multinomial class is ordered)
# estimateWeeklyProportion = function(admissions, ... ,nn=0.2, deg=2, cumulative = is.ordered(admissions$class)) {
#   # get the output as fractional weeks - we wil convert this to days later.
#   weeks = seq(min(admissions$admission_week),max(admissions$admission_week),by = 1/7)
#   
#   out = tibble()
#   # we are doing a binomial this for each level in the factor versus all other levels.
#   # this lets us create an estimate for multinomial data that I'm going to use later.
#   # I've never been sure about whether multinomial proportions can be treated as the sum of
#   # binomial 1 vs others, my suspicion is they can't, but I'm going to do it anyway 
#   for (level in levels(admissions$class)) {
#     if (cumulative) {
#       tmpdf = admissions %>% mutate(class_bool = class <= level)
#     } else {  
#       tmpdf = admissions %>% mutate(class_bool = class == level)
#     }
#     if (any(is.na(tmpdf$class_bool))) browser()
#     # detect some edge cases
#     if (nrow(tmpdf) == 0) {
#       # data set is empty
#       out = out %>% bind_rows(
#         tibble(
#           class = level,
#           admission_week = weeks,
#           admission_date = .weeks_to_date(weeks),
#           lower = 0,
#           median = 0,
#           upper = 1
#         )
#       )
#     } else if (!any(tmpdf$class_bool)) {
#       # for a given class there is no data or all observations are negative
#       out = out %>% bind_rows(
#         tibble(
#           class = level,
#           admission_week = weeks,
#           admission_date = .weeks_to_date(weeks),
#           lower = 0,
#           median = 0,
#           upper = 0
#         )
#       )
#     } else if (all(tmpdf$class_bool)) {
#       # for a given class all the observations are positive
#       out = out %>% bind_rows(
#         tibble(
#           class = level,
#           admission_week = weeks,
#           admission_date = .weeks_to_date(weeks),
#           lower = 1,
#           median = 1,
#           upper = 1
#         )
#       )
#     } else {
#       fit = locfit::locfit(class_bool ~ locfit::lp(admission_week,nn=nn,deg=deg),
#                            data = tmpdf,family="qbinomial", link="logit")
#       tmp = preplot(fit,newdata=weeks,se.fit = TRUE,band="local")
#       t = tmp$tr
#       out = out %>% bind_rows(
#         tibble(
#           class = level,
#           admission_week = weeks,
#           admission_date = .weeks_to_date(weeks),
#           lower = .opt(t(qnorm(0.05,tmp$fit,tmp$se.fit))),
#           median = t(tmp$fit), #only because fit is normally distributed so mean=median
#           upper = .opt(t(qnorm(0.95,tmp$fit,tmp$se.fit)))
#         )
#       )
#     }
#   }
#   out = out %>% mutate(class = factor(class, levels(admissions$class)))
#   return(out)
#   
# }
# 
# ## PANEL A Figure 1 - counts 
# 
# count_data = plot_data %>% 
#   group_by(Covid_induced_fac) %>%
#   group_modify( estimateWeeklyRate, nn=0.3 )
# 
# p1 = ggplot(plot_data,aes(x=admission_date,fill=survival30days))+
#   geom_histogram(binwidth = 7)+
#   facet_wrap(vars(Covid_induced_fac), nrow=1)+
#   scale_fill_manual(values = list("Survived"="#808080","Died"="#000000"), name="outcome")+
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %y", name=NULL)+
#   ylab("Weekly COPD admissions")+
#   theme_cowplot(font_size = 8)+
#   theme(
#     axis.text.x = element_text(angle = 60, hjust=1, vjust=1)
#   )+
#   geom_line(data = count_data, aes(x= admission_date, y=median), inherit.aes = FALSE, colour="red")+
#   geom_ribbon(data = count_data, aes(x= admission_date, ymin=lower,ymax=upper), inherit.aes = FALSE, fill = NA, colour="red", linetype = "dotted")+
#   geom_rug(data = variants_rug, mapping = aes(x=date, colour=variant), inherit.aes = FALSE,sides="t") +
#   coord_cartesian(xlim=range(plot_data$admission_date))+
#   geom_rect(data = variants, mapping = aes(xmin=start_date, xmax=end_date, ymin=-Inf,ymax=Inf), inherit.aes = FALSE,colour="white",fill="grey50",alpha=0.2, linetype="dotted")
# 
# #p1
# 
# ## Calculate proportions using a binomial / logit model smoother
# 
# ## PANEL B Figure 1 - proportion of patients requiring ventilation
# 
# # This is legacy for similar plot but by 30 day survival
# # proportion_data_1 = plot_data %>% 
# #   select(Covid_induced_fac, admission_week, class = survival30days) %>%
# #   group_by(Covid_induced_fac) %>%
# #   group_modify( estimateWeeklyProportion, nn=0.3 ) %>%
# #   # this calculates 
# #   filter(class == "Died") %>% mutate(measure = "Died within 30 days")
# 
# proportion_data_2 = plot_data %>% 
#   select(Covid_induced_fac, admission_week, class = vent_support) %>%
#   group_by(Covid_induced_fac) %>%
#   group_modify( estimateWeeklyProportion, nn=0.3 ) %>%
#   filter(class == "Required") %>% 
#   mutate(measure = "Ventilatory support")
# 
# # proportion_data_3 = plot_data %>% 
# #   select(Covid_induced_fac, admission_week, class = hospital_los_fac) %>%
# #   group_by(Covid_induced_fac) %>%
# #   group_modify( estimateWeeklyProportion, nn=0.3 ) %>%
# #   filter(class == "> 5 days") %>% mutate(measure = "LOS > 5 days")
# 
# # proportion_data = bind_rows(proportion_data_2) #,proportion_data_3)
# 
# p2 = 
#   ggplot(proportion_data_2, aes(x=admission_date))+
#   facet_wrap(vars(Covid_induced_fac), ncol=3)+
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %y", name=NULL)+
#   ylab("Proportion ventilated")+
#   scale_colour_brewer(palette = "Set2", name=NULL)+
#   theme_cowplot(font_size = 8)+
#   theme(
#     axis.text.x = element_text(angle = 60, hjust=1, vjust=1)
#   )+
#   geom_line(aes(y=median))+
#   geom_ribbon(aes(ymin=lower,ymax=upper), colour="black", linetype = "dotted", fill=NA)+
#   coord_cartesian(ylim=c(0,1),xlim=range(plot_data$admission_date))+
#   geom_rect(data = variants, mapping = aes(xmin=start_date, xmax=end_date, ymin=-Inf,ymax=Inf), inherit.aes = FALSE,colour=NA,fill="grey50",alpha=0.2)
# 
# p2
# 
# ## Survival trends over time as as survival surfaces
# 
# ## PANEL C Figure 1 - survival surface
# 
# # Death
# survival_data = plot_data %>%
#   mutate(
#     # If no date until death then assume 30 days.
#     # There is no censoring accounted for here. 
#     # We need to make sure all patients have at least 30 days follow up
#     survival_days = ifelse(!is.finite(survival_days),30,survival_days),
#     class = cut_integer(
#       survival_days,
#       cut_points = seq(4,29,4)
#     )
#   ) %>%
#   group_by(Covid_induced_fac) %>%
#   group_modify( estimateWeeklyProportion, nn=0.3, cumulative = TRUE )
# 
# p3 = ggplot(survival_data %>% mutate(class = forcats::fct_rev(class)), aes(x=admission_date,fill=class))+
#   facet_wrap(vars(Covid_induced_fac), nrow=1)+
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %y", name=NULL)+
#   ylab("Proportion survival")+
#   scale_fill_viridis_d(name="days survived")+
#   theme_cowplot(font_size = 8)+
#   theme(
#     axis.text.x = element_text(angle = 60, hjust=1, vjust=1)
#   )+
#   geom_area(aes(y=median),position="identity")+
#   coord_cartesian(ylim=c(0,1),xlim=range(plot_data$admission_date))+
#   geom_rect(data = variants, mapping = aes(xmin=start_date, xmax=end_date, ymin=-Inf,ymax=Inf), inherit.aes = FALSE,colour="white",fill="grey50",alpha=0.2, linetype="dotted")
# 
# p3
# 
# ## PANEL D Figure 1 - length of stay surface
# 
# survival_data_2 = plot_data %>%
#   # include only those with known LOS
#   # This does not btw do anything specific with people who died
#   filter(is.finite(hospital_length_of_stay)) %>%
#   mutate(
#     class = cut_integer(
#       hospital_length_of_stay,
#       cut_points=seq(4,29,4)
#     )
#   ) %>%
#   group_by(Covid_induced_fac) %>%
#   group_modify( estimateWeeklyProportion, nn=0.2, cumulative = TRUE )
# 
# p4 = ggplot(survival_data_2 %>% mutate(class = forcats::fct_rev(class)), aes(x=admission_date,fill=class))+
#   facet_wrap(vars(Covid_induced_fac), nrow=1)+
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %y", name=NULL)+
#   ylab("Proportion admissions")+
#   scale_fill_viridis_d(name="days admitted", option = "magma",direction = -1)+
#   theme_cowplot(font_size = 8)+
#   theme(
#     axis.text.x = element_text(angle = 60, hjust=1, vjust=1)
#   )+
#   geom_area(aes(y=median),position="identity")+
#   coord_cartesian(ylim=c(0,1),xlim=range(plot_data$admission_date))+
#   geom_rect(data = variants, mapping = aes(xmin=start_date, xmax=end_date, ymin=-Inf,ymax=Inf), inherit.aes = FALSE,colour="white",fill="grey50",alpha=0.2, linetype="dotted")
# 
# p4
# 
# fig1 = 
#   p1+theme(axis.text.x = element_blank())+
#   p2+theme(axis.text.x = element_blank())+
#   p3+theme(axis.text.x = element_blank())+
#   p4+
#   patchwork::plot_layout(ncol = 1,guides = "collect")+
#   patchwork::plot_annotation(tag_levels = "A")
# 
# fig1
# 
# ggplot2::ggsave(fig1,filename = "Figure1MultiPanelIncidence.png",width = unit(6,"inch"),height = unit(8,"inch"))
# 
