
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

.subtype_pal = function(x,...) {
  UseMethod(".subtype_pal",x)
}

# scales::show_col(.subtype_pal(scales::viridis_pal,direction=-1,subclasses = c(3,4,2))(12))
# scales::show_col(.subtype_pal(scales::hue_pal, h=c(-40,70) ,subclasses = c(3,4,2))(12))
.subtype_pal.function = function(x, ..., subclasses, undefined="#606060", lighten=NA) {
  dots = rlang::list2(...)
  dots = dots[names(dots) %in% names(formals(x))]
  pal_fun = rlang::exec(x, !!!dots)
  continuous = isTRUE(is.na(pal_fun(2)))
  return(function(n) {
    majors = if (n > sum(subclasses)) length(subclasses)+1 else min((1:length(subclasses))[cumsum(subclasses)>=n])
    tmp = subclasses[1:(majors-1)]
    minors = c(tmp,n-sum(tmp))
    pal_input = if(continuous) seq(0,1,length.out = majors-1) else (majors-1)
    major_pal = c(pal_fun(pal_input),undefined)
    out = purrr::map2(major_pal, minors, function(.x,.y) {
      if(is.na(lighten)) lighten = 1/.y
      fade = 1-(1-lighten)^((1:.y)-1)
      return(colorspace::lighten(.x,fade))
    }) %>% unlist()
    return(out)
  })
}

#scales::show_col(.subtype_pal(c("#FF0000","#00FF00","#0000FF"),subclasses = c(3,4,2))(12))
.subtype_pal.character = function(x, ..., subclasses, undefined="#606060", lighten=NA) {
  if (length(x) != length(subclasses)) stop("palette length and subclass length must be the same")
  major_pal = c(x,undefined)
  return(function(n) {
    majors = if (n > sum(subclasses)) length(subclasses)+1 else min((1:length(subclasses))[cumsum(subclasses)>=n])
    tmp = subclasses[1:(majors-1)]
    minors = c(tmp,n-sum(tmp))
    out = purrr::map2(major_pal, minors, function(.x,.y) {
      if(is.na(lighten)) lighten = 1/.y
      fade = 1-(1-lighten)^((1:.y)-1)
      return(colorspace::lighten(.x,fade))
    }) %>% unlist()
    return(out)
  })
}

scale_fill_subtype = function (.palette, subclasses, ..., undefined="#606060", lighten=NA,  na.value = "grey50", aesthetics = "fill") {
  dots = rlang::list2(...)
  discrete_scale_opts = dots[names(dots) %in% names(formals(ggplot2::discrete_scale))][!names(dots) %in% c("palette","scale_name")]
  p = .subtype_pal(.palette, ..., subclasses=subclasses, undefined = undefined, lighten = lighten)
  discrete_scale_opts = c(aesthetics=aesthetics, scale_name="subtype", palette=p, na.value = na.value, discrete_scale_opts)
  return(rlang::exec(discrete_scale, !!!discrete_scale_opts))
}

scale_colour_subtype = function( subclasses, class_colour = "#000000", subclass_colour = "#FFFFFF",  na.value = "grey50", aesthetics = "color", ...) {
  discrete_scale(
    aesthetics=aesthetics, 
    scale_name="subtype", 
    palette=function(n) {return(ifelse( c(TRUE,2:n %in% (cumsum(subclasses)+1)), class_colour, subclass_colour))}, 
    na.value = na.value,
    ...
  )
}

# install.packages("geomtextpath")

geom_timeseries_events = function(data,label_y = Inf,label_size=8,colour = "cyan", label_margin=0.5, ...) {
  dots = rlang::list2(...)
  if(!all(c("type","label","date") %in% colnames(data))) stop("missing columns in event dataframe. we need type (e.g. 'start','end',...), label, and date ")
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