
.clip_list  = function(x, ...) {
  UseMethod(".clip_list",x)
}

.escape_quote = function(x,quote="\"") {
  paste0(quote,stringr::str_replace_all(x, quote, paste0("\\",quote)), quote)
}

.clip_list.character = function(x,quote="\"") {
  out = x %>% unique() %>% .escape_quote %>% paste0(collapse="= NA,\n")
  out = sprintf("list(\n%s= NA\n)\n",out)
  out %>% clipr::write_clip()
  cat(out)
}

# install.packages("geomtextpath")

geom_timeseries_events = function(data,label_y = Inf,label_size=8,colour = "cyan", label_margin=0.5, ...) {
  dots = rlang::list2(...)
  if(!all(c("type","label","date") %in% colnames(data))) stop("missing columns in event dataframe. we need type (e.g. 'event','range',...), label, and date ")
  if(!all(c("y") %in% colnames(data))) data = data %>% mutate(y=label_y)
  # rects = events %>% filter(!is.na(`End date`))
  lines = data %>% filter(type == "event")
  return(list(
    # geom_rect(data=rects,mapping=aes(xmin=`Start date`,xmax=`End date`),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill="grey90",colour="grey90",alpha=0.5),
    # geomtextpath::geom_labelvline(
    #   data=lines,
    #   mapping=aes(xintercept = date, label=label),
    #   linetype="dashed",
    #   colour=colour, 
    #   fill = fill,
    #   hjust=0.95, show.legend = FALSE,
    #   label.padding = unit(0.05,"lines"),
    #   label.r = unit(0,"lines"),
    #   #offset = unit(-0.5,"lines"),
    #   halign="center",
    #   upright=FALSE,
    #   inherit.aes = FALSE,
    #   size=(labelSize/ggplot2:::.pt/(96/72))
    geom_vline(
      data=data,
      mapping=aes(xintercept = date),
      colour = colour,
      size=0.5,
      inherit.aes = FALSE,
      ...
    ),  
    geom_text(
      data=data,
      mapping=aes(x=date, y=y, label=label),
      # you won't get a better result that this unless labelvline is fixed.
      hjust="inward", 
      vjust=-0.5, # actually horizontal this creates a gap between the vertical text and the line
      angle=90,
      # nudge_x = -.5,
      # nudge_y = .5,
      #mapping=aes(x=date, y=Inf, label=label),
      #hjust = 1.1, vjust=1.5,
      #nudge_y = unit(0.05,"npc"),
      show.legend = FALSE,
      #fill = colour,
      colour = colour,
      #alpha = alpha,
      label.padding=unit(label_margin,"lines"),
      #label.r=unit(0,"npc"),
      # position=position_nudge(y=units(-0.05,"npc")),
      inherit.aes = FALSE,
      size=(label_size/ggplot2:::.pt/(96/72))
    )
  ))
}

# TODO: do the time series

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

# geom_timeseries_periods = function(data,label_y = Inf,label_size=8,colour = "cyan", label_margin=0.5, ...) {
#   dots = rlang::list2(...)
#   if(!all(c("type","label","date") %in% colnames(data))) stop("missing columns in event dataframe. we need type (e.g. 'event','range',...), label, and date ")
#   if(!all(c("y") %in% colnames(data))) data = data %>% mutate(y=label_y)
#   # rects = events %>% filter(!is.na(`End date`))
#   lines = data %>% filter(type == "event")
#   return(list(
#     # geom_rect(data=rects,mapping=aes(xmin=`Start date`,xmax=`End date`),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill="grey90",colour="grey90",alpha=0.5),
#     # geomtextpath::geom_labelvline(
#     #   data=lines,
#     #   mapping=aes(xintercept = date, label=label),
#     #   linetype="dashed",
#     #   colour=colour, 
#     #   fill = fill,
#     #   hjust=0.95, show.legend = FALSE,
#     #   label.padding = unit(0.05,"lines"),
#     #   label.r = unit(0,"lines"),
#     #   #offset = unit(-0.5,"lines"),
#     #   halign="center",
#     #   upright=FALSE,
#     #   inherit.aes = FALSE,
#     #   size=(labelSize/ggplot2:::.pt/(96/72))
#     geom_vline(
#       data=data,
#       mapping=aes(xintercept = date),
#       colour = colour,
#       size=0.5,
#       inherit.aes = FALSE,
#       ...
#     ),  
#     geom_text(
#       data=data,
#       mapping=aes(x=date, y=y, label=label),
#       # you won't get a better result that this unless labelvline is fixed.
#       hjust="inward", 
#       vjust=-0.5, # actually horizontal this creates a gap between the vertical text and the line
#       angle=90,
#       # nudge_x = -.5,
#       # nudge_y = .5,
#       #mapping=aes(x=date, y=Inf, label=label),
#       #hjust = 1.1, vjust=1.5,
#       #nudge_y = unit(0.05,"npc"),
#       show.legend = FALSE,
#       #fill = colour,
#       colour = colour,
#       #alpha = alpha,
#       label.padding=unit(label_margin,"lines"),
#       #label.r=unit(0,"npc"),
#       # position=position_nudge(y=units(-0.05,"npc")),
#       inherit.aes = FALSE,
#       size=(label_size/ggplot2:::.pt/(96/72))
#     )
#   ))
# }