# TODO:
# 1) Write lag analysis for estimators.
# Simple one for growth rate
# N.B. for Rt this will need wallinga et al Rt calculation of Rt using infectivity profile.
# 2) default estimator functions combining best practice.
# classes for estimates e.g. growthratetimeseries, rttimeseries, poissonratetimeserris, proportions
# simple timeseries as S3 class
# Multiple time series as S3
# 3) plotting functions
# consider a new library to hold class definitions and plotting functions
# or a new library to hold basic data manipulation and validation functions and a new library for plotting
# 4) restructure appendix vignettes to use latex versions from PhD and save figures somewhere. 
# rename outputs to remove dates
# check time-series plots still working / fix timeseries plots
# 5) re-architect validation functions
# into new package?

# a set of estimators for the simple single time series situations
# the estimates target a range of outputs such as poisson rate, proportion model, growth rate, etc.
# these are aimed to be tidy but assume (and enforce) column naming conventions are adhered to 
# these do not work on grouped data. they assume the input has been sanitised before hand, although tolerate NA values.
# The output of these will be Lambda (for the possion rate), Growth for the exponential growth rate

# make sure a given column exists and create it with the orElse function if not.
ensureExists = function(df, column, orElse = function(df) {stop("Missing column: ",column)},...) {
  out = df
  if(!(all(column %in% colnames(df)))) {
    out = orElse(df,...)
  }
  out
}

# make sure all the columns exist or report what the problems are
checkValid = function(df,columns) {
  success = all(sapply(columns, function(colname) {
    if(!(colname %in% colnames(df))) {
      message("Missing column: ",colname)
      return(FALSE)
    }
    return(TRUE)
  }))
  if(!success) stop("Invalid dataframe")
}

# create a weekday and is.weekend column
weekdayFromDates = function(df) {
  checkValid(df,"date")
  df %>% mutate(
    weekday = ordered(lubridate::wday(date),levels=1:7, labels=c("sun","mon","tue","wed","thur","fri","sat")),
    is.weekend = weekday %in% c("sat","sun")
  )
}

#' Calculates a weighting to apply to each day of week
#'
#' @param simpleTimeseries a covid timeseries data frame
#' @param ... 
#' @param valueVar the variable with the weekly periodicity
#'
#' @return the dataframe with a weekday.wt column which says how much that value is over expressed in the data 
weekendEffect = function(simpleTimeseries, valueVar="value", ...) {
  valueVar = ensym(valueVar)
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries = simpleTimeseries %>% weekdayFromDates()
  
  # set the default uniform weighting
  defaultWt = tibble(
    weekday = ordered(1:7,labels=c("sun","mon","tue","wed","thur","fri","sat")),
    weekday.wt = rep(1,7)
  )
  
  if(nrow(simpleTimeseries)>=21) {
  
    # if there is enough data estimate how much weight each day should have
    weight = simpleTimeseries %>% 
      mutate(.percentBias = 
              log(!!valueVar+1) / 
                slider::slide_dbl(log(!!valueVar+1), .before=3, .after=3,.f = mean, na.rm=TRUE,.complete = TRUE)-1
      ) %>% 
      group_by(weekday,.add=TRUE) %>% 
      summarise(
        weekday.wt = exp(abs(mean(.percentBias, na.rm=TRUE))),
        .groups="drop"
      ) %>% 
      mutate(weekday.wt=weekday.wt/mean(weekday.wt, na.rm=TRUE))
      
    if(nrow(weight) !=7 | any(is.na(weight$weekday.wt))) {
      weight = defaultWt
    }
  
  } else {
    weight = defaultWt
  }
      
  simpleTimeseries %>% inner_join(weight, by="weekday") %>% return()
}

# 
# #' @description Calculates a weighting to apply to each day of week
# #' @param simpleTimeseries a covid timeseries data frame
# #' @param window the window over which we are to normalise the sample size
# #' @param sampleSizeVar the variable with the sample size in it
# #' @return the dataframe with a sample.wt column which says how much that sample is relevant to the data
# sampleSizeEffect = function(simpleTimeseries, window, sampleSizeVar="total") {
#   if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#   sampleSizeVar = ensym(sampleSizeVar)
#   simpleTimeseries = simpleTimeseries %>% arrange(date) %>% mutate(
#     #sample.wt = ifelse(!!sampleSizeVar==0,0,!!sampleSizeVar / slider::slide_dbl(!!sampleSizeVar, .before = floor(window/2), .after = floor(window/2), mean, na.rm=TRUE,.complete = FALSE))
#     sample.wt = ifelse(!!sampleSizeVar==0,0,!!sampleSizeVar/mean(!!sampleSizeVar,na.rm = TRUE))
#   )
#   return(simpleTimeseries)
# }
# 


## Locfit estimate outputs ----


# This is just to format locfit results given a locfit model.
# extract the locfit result from the locfit model and format it
locfitExtractResult = function(df, model, estimate, modelName, link = "value") {
  
  tryCatch({
  
    points = preplot(model,where = "fitp",se.fit = TRUE,band="local")
  
    t = points$tr
    fit = points$fit
    se.fit = tryCatch({
      forecast::na.interp(points$se.fit)
    }, error = function(e) {
      rep(NA,length(fit))
    })
    
    df %>% formatResult(fit,se.fit,t,estimate,modelName,link)
    
  }, error = function(e) {
    
    df %>% nullResult(estimate,modelName,link,error = e$message) 
    
  })
}

# or else NA
opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

# format a transformed normally distributed variable into quantiles
formatResult = function(df, fit, se.fit, t, estimate, modelName,link) {
  df %>% mutate(
    !!(paste0(estimate,".",link)) := fit,
    !!(paste0(estimate,".SE.",link)) := se.fit,
    !!(paste0(estimate,".Quantile.0.025")) := opt(t(qnorm(0.025,fit,se.fit))), 
    !!(paste0(estimate,".Quantile.0.05")) := opt(t(qnorm(0.05,fit,se.fit))), 
    !!(paste0(estimate,".Quantile.0.25")) := opt(t(qnorm(0.25,fit,se.fit))), 
    !!(paste0(estimate,".Quantile.0.5")) := t(fit), 
    !!(paste0(estimate,".Quantile.0.75")) := opt(t(qnorm(0.75,fit,se.fit))), 
    !!(paste0(estimate,".Quantile.0.95")) := opt(t(qnorm(0.95,fit,se.fit))), 
    !!(paste0(estimate,".Quantile.0.975")) := opt(t(qnorm(0.975,fit,se.fit))), 
    !!(paste0(estimate,".model")) := modelName)
}

# extract the locfit result from the locfit model and format it
nullResult = function(df, estimate, modelName, link = "value", error = "unknown error", centralValue = 0) {
  df %>% formatResult(fit = centralValue, se.fit=NA_real_, t=function(x) x, estimate, modelName, link) %>%
    mutate(
      !!(paste0(estimate,".error")) := error
    )
}


#' Rename a growth rate estimate by placing a prefix in front o
#'
#' @param df the datafram with the Growth rate, possion rate, R_t or proportion estimates
#' @param prefix the prefix to add
#' @param estimates which estimates to rename (defaults to all of "Growth","Est","Proportion" and "Rt")
#'
#' @return the dataframe with the columns renamed
#' @export
renameResult = function(df, prefix, estimates = c("Growth","Est","Proportion","Rt","doublingTime")) {
  for (estimate in estimates) {
    df = df %>% rename_with(.cols = starts_with("Growth"), .fn = ~ paste0(prefix,".",.x))
  }
}

## Locfit estimators ----

# Generate the formula for a locfit model based on things I understand
locfitFormula = function(valueVar, nrowDf, window, polynomialDegree, nearestNeighbours = TRUE, ...) {
  valueVar=ensym(valueVar)
  tmp_alpha = min(window/nrowDf,1)
  tmp_alpha_2 = min((window*2+1)/nrowDf,1)
  lpParams = list(
    nn = if( nearestNeighbours ) tmp_alpha_2 else tmp_alpha, # this is given in fraction of total observations
    h = if( !nearestNeighbours ) window else 0, # this is given in units of X
    deg = polynomialDegree
  )
  lpParamsText = paste(names(lpParams),lpParams,sep="=",collapse=", ")
  lpFormula = as.formula(paste0(as_label(valueVar), " ~ locfit::lp(time, ",lpParamsText,")"))
  return(lpFormula)
}

#' Generate a smoothed estimate of the proportion of cases compared to some total.
#'
#' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' @param degree the polynomial degree
#' @param window the data window in days
#' @param estimateMean there is no closed form estimate of the mean of a logit transformed normal. it can be calculated by integration by this is relatively expensive and not done unless explicitly needed,
#' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit 
#'
#' @return a timeseries with binomial proportion estimates (columns starting with "Proportion")
#' @export
locfitProportionEstimate = function(simpleTimeseries, degree = 2, window = 14, estimateMean = FALSE,... ) { #, weightBySampleSize = FALSE, weightByWeekday = FALSE, ...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("total", orElse = function(ts,...) ts %>% mutate(total=1)) %>%
    #ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=total)) %>%
    #ensureExists("sample.wt", orElse = function(ts,...) ts %>% sampleSizeEffect(window=window, sampleSizeVar=total)) %>%
    ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
    mutate(.prop = ifelse(total==0,NA,value/total))
  
  if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model has values greater than 1. Did you specify total column correctly?")
  
  if(sum(na.omit(simpleTimeseries$.prop) != 0) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "logit", error = "not enough non zero values", centralValue = 0))
  }
  
  if(sum(na.omit(simpleTimeseries$.prop) != 1) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "logit", error = "not enough non unitary values", centralValue = 1))
  }
  
  # simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = 1)
  # if(weightBySampleSize) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*sample.wt)
  # if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*weekday.wt)
  # 
  # if(weightBySampleSize) {
  #   simpleTimeseries = simpleTimeseries %>% select(-.prop) %>% group_by_all() %>% summarise(
  #     .prop = c(rep(1,value),rep(1,total-value))
  #   )
  # }
  
  tryCatch({
    model = locfit::locfit(
      locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
      # weights = fit.wt,
      data=simpleTimeseries, 
      family="qbinomial",
      link="logit",
      ev=simpleTimeseries$time 
    )}, error=function(e) browser()
  )
  
  # weightLbl = case_when(
  #   weightBySampleSize & weightByWeekday ~ "both",
  #   weightBySampleSize ~ "sample",
  #   weightByWeekday ~ "weekday",
  #   TRUE ~ "none"
  # )
  
  weightLbl = "none"
  
  simpleTimeseries = simpleTimeseries %>% 
    locfitExtractResult(model, estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}:{weightLbl}"), link = "logit") %>% 
    select(-.prop)
  
  if (estimateMean) {
    simpleTimeseries = simpleTimeseries %>%
      mutate(
        Proportion.value = map2_dbl(Proportion.logit, Proportion.SE.logit, .f = ~ ifelse(is.na(.y),.x,logitnorm::momentsLogitnorm(.x,.y)[["mean"]])) #();NA_real_))
      )
  }
  
  return(simpleTimeseries)
}

#' Generate a smoothed estimate of the relative growth rate of cases compared to some baseline using proportions.
#'
#' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value, and total is assumed to be 1.
#' @param degree the polynomial degree
#' @param window the data window in days
#' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit 
#'
#' @return a timeseries with growth rate estimates (columns starting with "Growth")
#' @export
locfitProportionalGrowthEstimate = function(simpleTimeseries, degree = 2, window = 14, ...) { #}, weightBySampleSize = FALSE, weightByWeekday = FALSE, ...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("total", orElse = function(ts,...) {
      message("No total column in proportional timeseries - assuming value is a fraction, and total is 1.")
      ts %>% mutate(total=1)
    }) %>%
    # ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=total)) %>%
    # ensureExists("sample.wt", orElse = function(ts,...) ts %>% sampleSizeEffect(window=window, sampleSizeVar=total)) %>%
    ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
    mutate(.prop = ifelse(total==0,NA,value/total))
  
  if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model contains fractions greater than 1. Did you specify total column correctly?")
  
  if(sum(na.omit(simpleTimeseries$.prop) != 0) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Growth", modelName = glue::glue("binomial:{degree}:{window}"), link = "value",error = "not enough non zero values", centralValue = 0))
  }
  
  if(sum(na.omit(simpleTimeseries$.prop) != 1) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Proportion", modelName = glue::glue("binomial:{degree}:{window}"), link = "value", error = "not enough non unitary values", centralValue = 0))
  }
  
  # simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = 1)
  # if(weightBySampleSize) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*sample.wt)
  # if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*weekday.wt)
  
  model = locfit::locfit(
    locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
    # weights = fit.wt,
    data=simpleTimeseries, 
    family="qbinomial",
    link="logit",
    deriv=1,
    ev=simpleTimeseries$time 
  )
  
  # weightLbl = case_when(
  #   weightBySampleSize & weightByWeekday ~ "both",
  #   weightBySampleSize ~ "sample",
  #   weightByWeekday ~ "weekday",
  #   TRUE ~ "none"
  # )
  
  weightLbl = "none"
  
  # no link function in growth rate as the derivative
  simpleTimeseries %>% 
    locfitExtractResult(model = model, estimate = "Growth", modelName = glue::glue("binomial:{degree}:{window}:{weightLbl}"), link = "value") %>% 
    select(-.prop)
}

#' Generate a smoothed estimate of the absolute growth rate of cases using a poisson model.
#'
#' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' @param degree the polynomial degree
#' @param window the data window in days
#' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit 
#'
#' @return a timeseries with poisson rate estimates (columns starting with "Est")
#' @export
locfitPoissonRateEstimate = function(simpleTimeseries, degree = 2, window = 14, weightByWeekday = FALSE, ...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("weekday.wt", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value)) %>%
    ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
    mutate(.prop = value)
  
  if(sum(na.omit(simpleTimeseries$.prop) != 0) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Est", modelName = glue::glue("poisson:{degree}:{window}"), link = "log",error = "not enough non zero values", centralValue = 0))
  }
  
  simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = 1)
  if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*weekday.wt)
  
  model = locfit::locfit(
    locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
    weights = fit.wt,
    data=simpleTimeseries, 
    family="qpoisson",
    link="log",
    ev=simpleTimeseries$time 
  )
  
  weightLbl = case_when(
    weightByWeekday ~ "weekday",
    TRUE ~ "none"
  )
  
  # no link function in growth rate as the derivative
  simpleTimeseries %>% 
    locfitExtractResult(model, estimate = "Est", modelName = glue::glue("poisson:{degree}:{window}:{weightLbl}"), link="log") %>% 
    select(-.prop)
}

#' Generate a smoothed estimate of the absolute growth rate of cases using a poisson model.
#'
#' @param simpleTimeseries - a minimal time-series including date, value, and if available total. If total is present the proportion is value/total. otherwise it is value.
#' @param degree the polynomial degree
#' @param window the data window in days
#' @param ... may include "nearestNeigbour=FALSE" to disable the tail behaviour of locfit 
#'
#' @return a timeseries with growth rate estimates (columns starting with "Growth")
#' @export
locfitGrowthEstimate = function(simpleTimeseries, degree = 2, window = 14, weightByWeekday = FALSE, ...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("weekday.wt", orElse = function(ts,...) weekendEffect(ts,valueVar=value)) %>%
    ensureExists("time", orElse = function(ts,...) mutate(ts, time = as.integer(date-max(date)))) %>%
    mutate(.prop = value)
  
  if(sum(na.omit(simpleTimeseries$.prop) != 0) < degree) {
    return(simpleTimeseries %>% nullResult(estimate = "Growth", modelName = glue::glue("poisson:{degree}:{window}"), link = "value",error = "not enough non zero values", centralValue = 0))
  }
  
  simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = 1)
  if(weightByWeekday) simpleTimeseries = simpleTimeseries %>% mutate(fit.wt = fit.wt*weekday.wt)
  
  model = locfit::locfit(
    locfitFormula(.prop, nrowDf = nrow(simpleTimeseries), window, degree, ...),
    weights = fit.wt,
    data=simpleTimeseries, 
    family="qpoisson",
    link="log",
    deriv=1,
    ev=simpleTimeseries$time 
  )
  
  weightLbl = case_when(
    weightByWeekday ~ "weekday",
    TRUE ~ "none"
  )
  
  # no link function in growth rate as the derivative
  simpleTimeseries %>% 
    locfitExtractResult(model = model, estimate = "Growth", modelName = glue::glue("poisson:{degree}:{window}:{weightLbl}"), link = "value") %>% 
    #TODO: more statistics here?
    select(-.prop)
}


#' Calucualte a doubling time with quantiles for any timeseries with Growth rate estimates
#'
#' @param simpleTimeseries 
#'
#' @return  a timeseries with doubling time estimates (columns starting with "doublingTime")
#' @export
doublingTimeFromGrowthRate = function(simpleTimeseries) {
  reorder = function(x) (1-(stringr::str_extract(x,"[0-9]\\.[0-9]+") %>% as.numeric())) %>% sprintf(fmt="doublingTime.Quantile.%1.3g")
  simpleTimeseries %>% mutate(across(.cols = starts_with("Growth.Quantile"), .fns = ~ log(2)/.x, .names = "{reorder(.col)}"))
}


#' Calculate a reproduction number estimate using the Wallinga 2007 estimation using empirical generation time distribution. This uses resampling to transmit uncertainty in growth rate estimates
#'
#' @param simpleTimeseries - With a "Growth" estimate as a normally distributed quantility
#' @param yMatrix - the matrix of possible infectivity profiles as discrete distributions
#' @param aVector - the upper boundaries of the time cut-offs for the infectivity profiles
#' @param bootstraps - the number of bootstraps to take to calculate for each point.
#' @param quantiles - quantiles to calculate.
#' @import logitnorm
#'
#' @return a timeseries with "Rt" estimates
#' @export
rtFromGrowthRate = function(simpleTimeseries, infectivityProfile, yMatrix = infectivityProfile$yMatrix, aVector = infectivityProfile$aVector, bootstraps = 20*dim(yMatrix)[2], quantiles = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  simpleTimeseries %>% checkValid(c("date","value","Growth.value","Growth.SE.value"))
  
  # grab the y matrix from the list column
  y_cols = matrix(yMatrix)
  a = aVector
  # figure out how many bootstraps we need:
  bootsPerInf = max(c(bootstraps %/% dim(yMatrix)[2],1))
  # lose the zero values in y and a, if present (which they will be):
  if (a[1]==0) {
    y_cols = matrix(y_cols[-1,])
    a = a[-1]
  }
  # get the infectivity profiles as a list of vectors, each bootstrap profile will be a vector.
  ys = asplit(y_cols, MARGIN=2)
  
  d3 = simpleTimeseries %>% mutate(R = map2(Growth.value, Growth.SE.value, function(mean_r,sd_r) {
      
    #r_samples = rnorm(bootsPerInf*length(ys),mean_r,sd_r)
    qnts = seq(0,1,length.out = bootsPerInf)[2:(bootsPerInf-1)]
    r_samples = qnorm(p=qnts,mean_r,sd_r)
    rs = asplit(matrix(r_samples,nrow=length(ys)), MARGIN=1)
    # browser()
    out = map2(rs,ys,function(r10,y) {
      # browser()
      R10 = sapply(r10, function(r) {
        # browser()
        R = r/sum(y*(exp(-r*lag(a,default=0))-exp(-r*a))/(a - lag(a,default=0)))
      })
    })
    R_out = as.vector(sapply(out,c))
    R_q = quantile(R_out, quantiles)
    names(R_q) = paste0("Rt.Quantile.",quantiles)
    R_summ = enframe(R_q) %>% pivot_wider() %>% mutate(Rt.value = mean(R_out), Rt.SE.value = sd(R_out))
    return(R_summ)
  }))
    
  return(d3 %>% unnest(R) %>% mutate(Rt.model = "wallinga:growth-rate"))
}

## Manchester growth rate ----
# ## Adapted from code
# Copyright (c) 2020 Ian Hall
# See LICENCE for licensing information

# Growth rate estimates for confirmed cases in Europe and for different metrics in Italy using GAM
# Figure 1 (main text) and figures S1 and S2 (electronic supplementary material) of:
# 
# Pellis L, Scarabel F, Stage HB, Overton CE, Chappell LHK, Fearon E, Bennett E, 
# University of Manchester COVID-19 Modelling Group, Lythgoe KA, House TA and Hall I, 
# "Challenges in control of COVID-19: short doubling time and long delay to effect of interventions", 
# Philosophical Transactions of the Royal Society B (2021)
#
# gamGrowthEstimate = function(simpleTimeseries, meth="GCV.Cp", FE='WD'){
#   if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#   
#   simpleTimeseries %>% checkValid(c("date","value"))
#   simpleTimeseries = simpleTimeseries %>% 
#     arrange(date) %>%
#     ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
#     mutate(.incidence = value)
#   
#   #res <- data.frame(sdt=rep(0,npts),sdtup=rep(0,npts),sdtlow=rep(0,npts),doub=rep(0,npts),doubup=rep(0,npts),doublow=rep(0,npts))
#   #Tv <- timev
#   
#   if(FE=='None') {
#     MGAM <- mcgv::gam(.incidence ~ mcgv::s(time), data = simpleTimeseries, family=quasipoisson, method=meth)
#   } else {
#     simpleTimeseries = simpleTimeseries %>% 
#       ensureExists("weekday", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value)) %>%
#       ensureExists("is.weekend", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value))
#     if(FE=='WE'){
#       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+is.weekend, data = simpleTimeseries, family=quasipoisson, method=meth)
#     } else {
#       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+weekday, data = simpleTimeseries, family=quasipoisson, method=meth)
#     }
#   }
#   
#   X0 <- predict(MGAM, simpleTimeseries %>% mutate(time=time-eps), type="lpmatrix")
#   eps <- 1e-7 ## finite difference interval
#   X1 <- predict(MGAM, simpleTimeseries %>% mutate(time=time+eps),type="lpmatrix")
#   Xp <- (X1-X0)/(2*eps) ## maps coefficients to (fd approx.) derivatives
#   # something to do with extracting the coefficients
#   off <- ifelse(FE=='None',1,ifelse(FE=='WE',2,7))  
#   Xi <- Xp*0 
#   Xi[,1:9+off] <- Xp[,1:9+off] ## weekend Xi%*%coef(MGAM) = smooth deriv i
#   df <- Xi%*%coef(MGAM)              ## ith smooth derivative 
#   df.sd <- rowSums(Xi%*%MGAM$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
#   ## derivative calculation, pers comm S. N. Wood, found in mgcv:  Mixed  GAM  Computation  Vehicle  with  automatic  smoothness  estimation.  R  packageversion 1.8-31 (2019) https://CRAN.R-project.org/package=mgcv.
#   
#   simpleTimeseries %>% formatResult(fit = df, se.fit = df.sd,t = function(x) x, estimate = "Growth", modelName = glue::glue("poisson:gam-{meth}:{FE}"), link = "value")
#   
# }

## Point estimators ----


#' Calculate a slightly more robust estimate of growth rate and proportion based on a single model at a range of
#'
#' @param simpleTimeseries - the timeseries containing date, value and total
#' @param dates - dates at which to evaluate the model
#' @param window - the window of data 
#' @param weekly - either "weekday","weekend" or "none" to define whether to fit a fixed effect model to the weekday, or the is.weekend
#' @param includeModel - keep the fitted model as a list column for fit analysis
#' @param ... 
#'
#' @return a dataframe of evaluation dates, growth rates, proportions and model fit
#' @export
pointProportionEstimate = function(simpleTimeseries, dates = max(simpleTimeseries$date)-3, window = 14, weekly = "weekday", includeModel = TRUE,...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  if (find.package("logitnorm", quiet = TRUE) %>% length %>% equals(0)) {
    message("Installing logitnorm needed for analyses")
    install.packages("logitnorm", repos = "https://cloud.r-project.org")
  }
  
  predictDates = as.Date(dates)
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("total", orElse = function(ts,...) ts %>% mutate(total=1)) %>%
    ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
    ensureExists(c("weekday","is.weekend"), orElse = function(ts,...) ts %>% weekdayFromDates()) %>%
    mutate(.prop = ifelse(total==0,NA,value/total))
  
  if(any(simpleTimeseries$.prop > 1,na.rm = TRUE)) stop("Proportions model has values greater than 1. Did you specify total column correctly?")
  
  if (weekly=="weekday") {
    modelFormula = .prop ~ time + weekday
  } else if (weekly=="weekend") {
    modelFormula = .prop ~ time + is.weekend
  } else {
    modelFormula = .prop ~ time
  }
  
  bind_rows(lapply(predictDates, function(predictDate) {
    
    dateMin = as.Date(predictDate)-floor(window/2)
    dateMax = as.Date(predictDate)+floor(window/2)
    
    suppressWarnings({
      model = glm(
        modelFormula,
        data=simpleTimeseries %>% filter(date >= dateMin & date <= dateMax) %>% mutate(sample.wt = total/mean(total,na.rm=TRUE)), 
        family="binomial",
        weights=sample.wt
      )
    })
    
    predictAt = tibble(
      date = predictDate,
      time = as.integer(date-max(simpleTimeseries$date)),
    ) %>% weekdayFromDates()
    
    predicted = predict(model,newdata = predictAt,se.fit = TRUE, type="link")
    linkFn = family(model)$linkinv
    
    predictAt = formatResult(predictAt, unname(predicted$fit), unname(predicted$se.fit), linkFn, "Proportion", "glm", "logit")
    predictAt = predictAt %>% mutate(
      Proportion.value = map2_dbl(Proportion.logit, Proportion.SE.logit, .f = ~ logitnorm::momentsLogitnorm(.x, .y)[["mean"]])
    )
    gr = summary(model)$coefficients["time",]
    predictAt = formatResult(predictAt, gr[[1]], gr[[2]], function(x) x, "Growth", "glm", "value")
    
    if(includeModel) predictAt %>% mutate(fit = list(model))
    
  }))
  
}

#' Calculate a slightly more robust estimate of growth rate and  poisson rate based on a single model
#'
#' @param simpleTimeseries - the timeseries containing date and value
#' @param dates - dates at which to evaluate the model
#' @param window - the window of data 
#' @param weekly - either "weekday","weekend" or "none" to define whether to fit a fixed effect model to the weekday, or the is.weekend
#' @param includeModel - keep the fitted model as a list column for fit analysis
#' @param ... 
#'
#' @return a dataframe of evaluation dates, growth rates, poisson rates and model fit
#' @export
pointPoissonEstimate = function(simpleTimeseries, dates, window, weekly = "weekday", includeModel = TRUE,...) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  predictDates = as.Date(dates)
  
  simpleTimeseries %>% checkValid(c("date","value"))
  simpleTimeseries = simpleTimeseries %>% 
    arrange(date) %>%
    ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
    ensureExists(c("weekday","is.weekend"), orElse = function(ts,...) ts %>% weekdayFromDates()) %>%
    mutate(.prop = value)
  
  if (weekly=="weekday") {
    modelFormula = .prop ~ time + weekday
  } else if (weekly=="weekend") {
    modelFormula = .prop ~ time + is.weekend
  } else {
    modelFormula = .prop ~ time
  }
  
  bind_rows(lapply(predictDates, function(predictDate) {
    
    dateMin = as.Date(predictDate)-floor(window/2)
    dateMax = as.Date(predictDate)+floor(window/2)
    
    model = glm(
      modelFormula,
      data=simpleTimeseries %>% filter(date >= dateMin & date <= dateMax), 
      family="poisson"
    )
    
    predictAt = tibble(
      date = predictDate,
      time = as.integer(date-max(simpleTimeseries$date)),
    ) %>% weekdayFromDates()
    
    predicted = predict(model,newdata = predictAt,se.fit = TRUE, type="link")
    linkFn = family(model)$linkinv
    
    predictAt = formatResult(predictAt, unname(predicted$fit), unname(predicted$se.fit), linkFn, "Est", "glm", "log")
    gr = summary(model)$coefficients["time",]
    predictAt = formatResult(predictAt, gr[[1]], gr[[2]], function(x) x, "Growth", "glm", "value")
    
    if(includeModel) predictAt %>% mutate(fit = list(model))
  }))
  
}


## EpiEstim wrapper ----
#' Minimal epiestim wrapper to execute a time series R_t using a discrete infectivity profile matrix, and format the result to be consistent with the rest of this..
epiestimRtEstimate = function(simpleTimeseries, yMatrix, bootstraps = 10*dim(yMatrix)[2], window = 14) {
  if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
  
  siConfig = EpiEstim::make_config(method = "si_from_sample")
  tmp = simpleTimeseries %>% dplyr::select(dates=date,I=value)
  
  simpleTimeseries = simpleTimeseries %>% dplyr::mutate(seq_id=row_number())
  bootsPerInf = max(c(bootstraps %/% dim(yMatrix)[2],1))
  
  siConfig$t_start = c(2:(nrow(tmp)-window))
  siConfig$t_end = siConfig$t_start+window
  siConfig$n2 = bootsPerInf
  
  warn = NA
  
  tmp4 = 
    withCallingHandlers(
      tryCatch(EpiEstim::estimate_R(tmp, method = "si_from_sample",config=siConfig,si_sample = yMatrix), error = stop), warning= function(w) {
        warn <<- w$message
        invokeRestart("muffleWarning")
      })
  tmp5 = tmp4$R %>% mutate(seq_id=t_end, errors=NA, `Rt.window`=window) #warn)
  tmp5 = tmp5 %>% rename_with(.cols = contains("(R)"),.fn=function(x) paste0("Rt.",stringr::str_remove(x,fixed("(R)")))) %>%
    rename(`Rt.Quantile.0.5` = Rt.Median)
  tmp6 = simpleTimeseries %>% dplyr::left_join(tmp5, by="seq_id")
  return(tmp6 %>% select(-seq_id))

}

# plotProportionEstimate = function(simpleTimeseries, mapping = aes(), ...) {
#   
#   simpleTimeseries = simpleTimeseries %>% ensureExists("Proportion.Quantile.0.5", orElse = estimateProportions(simpleTimeseries,...))
#   # We are going to pretend there is just one 
#   simpleTimeseries
#   tmp2 = tmp %>% filter(date <= max(date)-1) %>% mutate(
#     binom::binom.confint(Negative,n,method="wilson")
#   )
#   
#   ggplot(estimate,aes(x=date,y=fit,ymin=lo,ymax=hi))+geom_ribbon(alpha=0.3)+geom_line(colour="blue")+
#     geom_point(data=tmp2,mapping=aes(x=date,y=mean),inherit.aes = FALSE)+
#     geom_errorbar(data=tmp2,mapping=aes(x=date,ymin=lower,ymax=upper),inherit.aes = FALSE)+
#     scale_y_continuous(trans = "logit")
# }
