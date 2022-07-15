# Import AVONCAP data and cleanse into a tidy data format
# assuming there will be changes to the format we would like to be able to make
# the import reverse compatible to previous versions of the data.


here::i_am("data-wrangling.R")

## File handling functions ----

# lists all the csv files in the input directory
# parses the 
all_files = function() {
  files = fs::dir_ls(here::here("input"),recurse = TRUE) %>% stringr::str_subset(".+\\.csv")
  info = fs::file_info(files) %>% mutate(filename = path %>% fs::path_ext_remove() %>% fs::path_file() %>% stringr::str_extract("[a-zA-Z\\s]+") %>% trimws())
  info %>% 
    mutate(
      modification_date = as.Date(modification_time),
      filename_date = path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_extract("\\d+") %>% stringr::str_replace("^(\\d{4})(\\d{2})$","\\120\\2") %>% as.Date(format="%d%m%Y"),
      hospital = case_when(
        stringr::str_detect(path,"_NBT_") ~ "NBT",
        stringr::str_detect(path,"_BRI_") ~ "BRI",
        TRUE ~ NA_character_
      ),
      study_year = (path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_match("_(y1|y2|y3)_"))[,2],
      date = pmin(modification_date,filename_date,na.rm = TRUE)
    ) %>%
    select(filename, path, date, hospital, study_year)
}

most_recent_files = function(fname, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))) {
  all_files() %>%
    filter(filename == fname & date <= reproduce_at) %>% 
    group_by(hospital,study_year) %>% 
    # given a particular hospital or study year combo find he most recent file
    filter(date == max(date)) %>% 
    # is this file a single file on a particular date with no version, or a versioned set of files (originally this was trust specific but also happens when it is year specific?
    mutate(byTrust = !is.na(hospital)) %>% 
    # get the most recent file or set of files if it is versioned.
    group_by(byTrust) %>% 
    mutate(latest_date = max(date)) %>% ungroup() %>%
    filter(latest_date == max(latest_date)) %>%
    ungroup() %>%
    select(-byTrust)
}

# detecting errors when merging dataframes ----

.detect_parse_issues = function(listOfDf) {
  suppressWarnings(purrr::map(listOfDf, ~ problems(.x)))
}

.detect_structure =  function(listOfDf) {
  purrr::map(listOfDf, ~ inner_join(
    .x %>% lapply(class) %>% unlist() %>% enframe() %>% rename(type = value),
    .x %>% lapply(function(c) all(is.na(c))) %>% unlist() %>% enframe() %>% rename(empty = value),
    by = "name"
  ))
}

.detect_empty = function(structure) {
  structure %>% purrr::map(~ .x %>% filter(empty) %>% pull(name))
}

.detect_mismatch = function(structure) {
  data = tibble(index = 1:length(structure), structure = structure) %>% unnest(structure)
  majority = data %>% filter(!empty) %>% group_by(name,type) %>% count() %>% group_by(name) %>% filter(n == max(n))
  structure %>% purrr::map(~ .x %>% filter(!empty) %>% anti_join(majority, by=c("name","type")) %>% pull(name))
}

save_data_source_info = function(rawDataDf, out) {
  readr::write_lines(x = c(
    attr(rawDataDf,"file"), 
    digest::digest(rawDataDf, algo="md5")), file = out("data_sources.txt"))
}

#' Minimal function to load the AvonCAP data 
#' 
#' without making a whole lot of assumptions about where the data resides.
#' This does not check data integrity. It will probably complain if the csv files have different 
#' column names but I don't guarantee it. It uses readr::read_csv
#' 
#' Looking at this it will all need to be refactored but hopefully this is enough for the time being
#'
#' @param files a vector of full file names for all the data you want to load e.g. c("C:/files/DeltaVE_2021","C:/files/DeltaVE_2022")
#' @param hospitals a vector of version data for each file as a string e.g. c("NBT_y1","y2"), NA if none
#' @param year a vector of year data for each file (as "y1", "y2")
#'
#' @return a full data frame of whatever was in the files plus a version column (usually y1 etc), and a file column (with the filename).
minimal_loader = function(files, hospitals, years) {
  
  maxDate = file.info(files) %>% pull(ctime) %>% max() %>% as.Date()
  
  tmp = tibble(hospital = hospital, study_year = years, file = files)
  tmp %>% 
    mutate(csv = purrr::map(path, ~ readr::read_csv(.x, na = c("","NA","Na","na","N/A","UNK"), show_col_types = FALSE))) %>%
    unnest(csv)
  
  attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
  attr(tmp,"date") = unique(maxDate)
}



# requires filenames to be of the format:
# TYPE_HOSPITAL_YEAR_DATE.csv
# where TYPE is going to be the data source identifier (e.g. DeltaVE), HOSPITAL is BRI or NBT, YEAR is y1,y2 or y3, and DATE is ISO date (YYYY-MM-DD)
load_data = function(filename, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date())), merge = TRUE) {
  if(reproduce_at != Sys.Date()) warning("REPRODUCING RESULTS FROM: ",reproduce_at, ", to disable this set options(reproduce.at=NULL)")
  tmp = filename
  # decide whether files are trust by trust or a single combined file
  tmp_files = most_recent_files(filename, reproduce_at)
  if(nrow(tmp_files) == 0) stop("No suitable files matching ",filename,", before date: ",reproduce_at)
  tmp2 = ggrrr::cached({
    files = tmp_files %>% pull(path)
    if (length(files)>1 & length(unique(tmp_files$date)) > 1) {
      warning("The most recent versions of the inputs have different effective dates: ",filename," i.e. ", paste0(files,collapse = ";"))
      warning("This could mean the files are out of sync.")
    }
    
    # TODO: This is super slow because it checks all the data types of everything
    # on the larger dataset it throws parse errors because it cannot guess the correct type
    # There is an argument for reading it all in as text then analysing and checking the type when normalising.
    # This would need a rethink on the way in which structural issues are dealt with.
    
    data = tmp_files %>% 
      mutate(csv = purrr::map(path, ~ suppressWarnings(readr::read_csv(.x, na = c("","NA","Na","na","N/A","UNK"), show_col_types = FALSE))))
    data2 = data %>% 
      mutate(file = fs::path_file(path)) %>%
      mutate(parse_issues = .detect_parse_issues(csv)) %>%
      # get rid of all parsing metadata
      mutate(csv = purrr::map(csv, ~ as_tibble(.x))) %>%
      mutate(structure = .detect_structure(csv)) %>%
      mutate(empty = .detect_empty(structure)) %>%
      mutate(mismatches = .detect_mismatch(structure)) %>%
      select(-path,-filename)
    
    col_suppress = unique(c(unlist(data2$mismatches)))
    
    if(length(col_suppress) > 0) {
      warning("INCONSISTENT COLUMN(S) IN FILES: ",paste0(col_suppress,collapse=";"))
      warning("NOT MERGING FILES")
      merge = FALSE
    }
    
    if (nrow(bind_rows(data2$parse_issues))>0) {
      message(nrow(bind_rows(data2$parse_issues))," parse issues in raw files. Check the parse_issues attrbute.")
    }
    
    if (!merge) {
      return(data2 %>% select(hospital,year,file,csv, mismatches, empty, structure, parse_issues))
    }
    
    tmp = data2 %>% 
      # Lose empty columns which will have been assigned incorrect type
      mutate(csv = purrr::map2(csv, empty, ~ .x %>% select(-any_of(.y)))) %>%
      # Lose conflicting data type columns
      mutate(csv = purrr::map2(csv, mismatches, ~ .x %>% select(-any_of(.y)))) %>% 
      # force merge the files together
      select(hospital,study_year,file,csv) %>% 
      unnest(csv)
    
    attr(tmp,"parse_issues") = bind_rows(data2$parse_issues)
    attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
    attr(tmp,"date") = unique(tmp_files$latest_date)
    tmp
  },filename, reproduce_at, merge, tmp_files, .cache = here::here("input/cache"))
  return(tmp2)
}

## Data normalisation infrastructure ----


normalise_data = function(
    rawData, 
    ethnicityFiles = c(
      here::here("input/20-21 ethnicity data 120722.csv"),
      here::here("input/21-22 ethnicity data 120722.csv")
    ), 
    remove_mapped = TRUE, 
    remove_unmapped = TRUE, 
    mappings=.mappings, 
    messages = c("files: {files}","{.total} rows","analysis from: {reproduce_at}","files from: {date}"),
    ...
) {
  
  reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))
  files = attr(rawData,"file")
  date = attr(rawData,"date")
  
  try({
    ethn = bind_rows(lapply(ethnicityFiles, readr::read_csv))
    rawData = rawData %>% left_join(ethn, by="record_number", suffix = c("",".ethn"))
  })
    
  ggrrr::cached({
    tmp = rawData
    if ("admission_date" %in% colnames(tmp)) {
      # the NHS data set has and admission date
      tmp = tmp %>% 
        mutate(
          year = lubridate::year(admission_date),
          week_number = lubridate::epiweek(admission_date),
          study_week = study_week(admission_date)
        ) 
    } else {
      tmp = tmp %>%
        # the Bristol data set has week_number which is a week number from start of the study in that year.
        # The study starts on week 31. Therefore for the 20-21 database weeks 31-52 are in 2020 and 0-30 are in 2021
        # we get the year from the file-name itself (assuming it was correctly named)
        mutate(
          year = case_when(
            is.numeric(year) & !is.na(year) ~ year,
            study_year == "y1" & week_number>30 ~ 2020, 
            study_year == "y1" & week_number<=30 ~ 2021,
            study_year == "y2" & week_number>30 ~ 2021, 
            study_year == "y2" & week_number<=30 ~ 2022,
            study_year == "y3" & week_number>30 ~ 2022, 
            study_year == "y3" & week_number<=30 ~ 2023,
            TRUE ~ NA_real_
          )) %>%
        mutate(
          study_week = (year-2020)*52+week_number-1,
          admission_date = start_date_of_week(study_week),
          week_number = lubridate::epiweek(admission_date)
        )
    }
    
    if (!"hospital" %in% colnames(tmp)) {
      # The hospital mak be known from the file the data came from. If not we can work it out from
      # the record number
      tmp = tmp %>% mutate(hospital = case_when(
        tolower(substr(record_number,1,1)) == "b" ~ "BRI",
        tolower(substr(record_number,1,1)) == "n" ~ "NBT",
        TRUE ~ NA_character_
      ))
    }
    
    originalColnames = colnames(tmp)
    # prefix all the original columns with a single "."
    tmp = tmp %>% rename_with(.fn= ~ paste0(".",.x))
    
    # mutate field values into new values
    for(i in 1:length(mappings)) {
      mappingName = names(mappings)[[i]]
      tryCatch({
        .fn = mappings[[i]]
        # add in the "."
        tmp = tmp %>% .fn(paste0(".",mappingName))
      }, error = function(e) {
        warning("could not process column or column set ",mappingName," due to ",e$message)
      })
    }
    
    fileName = files
    
    message("mapped ",tmp %>% select(starts_with("..")) %>% colnames() %>% length()," columns")
    if(remove_mapped) {
      tmp = tmp %>% select(-starts_with(".."))
    } else {
      message("renamed original columns as: ",tmp %>% select(starts_with("..")) %>% colnames() %>% paste0(collapse="; "))
    }
    
    unmappedCols = tmp %>% select(c(starts_with("."), -starts_with(".."))) %>% colnames()
    
    message("Did not map ", unmappedCols %>% length()," columns")
    
    # detect multiple admission episodes based on nhs_number, if present
    tryCatch({
      tmp = tmp %>% group_by(.nhs_number) %>% arrange(admission.date) %>% 
        mutate(
          admission.episode = row_number(),
          admission.interval = as.integer(admission.date-lag(admission.date))
        ) %>% ungroup()
    }, error = function(e) warning("could not identify duplicates by NHS number"))
    
    if (!"admission.episode" %in% colnames(tmp)) tmp = tmp %>% mutate(admission.episode = NA_real_)
    
    if(remove_unmapped) {
      tmp = tmp %>% select(!(c(starts_with("."), -starts_with(".."))))
    } else {
      message("renamed unmapped columns as: ",unmappedCols %>% paste0(collapse="; "))
    }
    
    tmp %>% track(.messages = messages) %>% capture_exclusions()
    
  }, rawData, mappings, remove_mapped, remove_unmapped, .cache = here::here("input/cache"), ...)
  # return(tmp2)
}

# Allows a 
.normalise_variant = function(renameTo) {
  renameTo = ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    df %>% mutate(!!renameTo := case_when(
        !!valueCol %>% stringr::str_detect("(P|p).*(R|r)") ~ "Delta",
        !!valueCol %>% stringr::str_detect("(K|k).*(N|n)") ~ "Omicron",
        is.na(!!valueCol) ~ NA_character_,
        TRUE ~ "unknown"
      ) %>% factor(levels = c("unknown","Delta","Omicron"))) %>% 
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

# TODO: use labelled:: for column names

.normalise_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    df %>% 
      mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_yesno = function(renameTo) {
  #TODO: error checking
  .normalise_list({{renameTo}}, c("no","yes"), zeroValue=TRUE)
}

.normalise_name = function(renameTo) {
  renameTo=ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>% 
      mutate(!!renameTo := !!valueCol) %>%
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_double = function(renameTo, limits=c(-Inf,Inf)) {
  renameTo=ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>% 
      mutate(!!renameTo :=  suppressWarnings(as.numeric(!!valueCol))) %>%
      mutate(!!renameTo :=  if_else(!!renameTo < limits[1] | !!renameTo > limits[2],NA_real_,!!renameTo)) %>%
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_text_to_factor = function(renameTo, levels) {
  renameTo=ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>% 
      mutate(!!renameTo := suppressWarnings(factor(tolower(!!valueCol), levels=tolower(levels), labels=levels))) %>%
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_date = function(renameTo, limits=as.Date(c("2020-01-01","2030-01-01")), tryFormats="%d/%m/%Y") {
  renameTo=ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>% 
      mutate(!!renameTo :=  suppressWarnings(as.Date(!!valueCol, tryFormats = tryFormats))) %>%
      mutate(!!renameTo :=  if_else(!!renameTo < limits[1] | !!renameTo > limits[2],as.Date(NA),!!renameTo)) %>%
      rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_checkboxes = function(renameToVars) {
  return(function(df, valueColPrefix) {
    
    i=1
    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = as_label(naCol) %in% colnames(df)
    for(renameTo in renameToVars) {
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",i))
      message("mapping ",valueCol," to ",renameTo)
      # rename original ___1, ___2, etc to something meaningful and convert to ordered factor
      df = df %>% mutate(!!renameTo := !!valueCol)
      if(hasNa) {
        # deal with ___na columns etc, by making the renamed checkbox variables have an NA in them for the values where NA has been checked
        df = df %>% mutate(!!renameTo := if_else(!!naCol == 1, NA_real_, !!renameTo))
      }
      df = df %>% mutate(!!renameTo := !!renameTo %>% factor(levels=c(0,1), labels=c("no","yes")))
      # hide original ___1, ___2, etc columns
      df = df %>% rename(!!(paste0(".",valueCol)) := (!!valueCol))
      #TODO: validation checking?
      i=i+1
    }
    # hide original ___na columns
    if(hasNa) df = df %>% rename(!!(paste0(".",naCol)) := (!!naCol))
    
    return(df)
    
  })
}

.normalise_checkboxes_to_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=ensym(renameTo)
  # return(function(df, valueCol) {
  #   valueCol = as.symbol(valueCol)
  #   message("mapping ",valueCol," to ",renameTo)
  #   df %>% 
  #     mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
  #     rename(!!(paste0(".",valueCol)) := (!!valueCol))
  # })

  return(function(df, valueColPrefix) {
    
    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = as_label(naCol) %in% colnames(df)
    
    df = df %>% mutate(!!renameTo := as(NA,class(values)))
    
    for(i in 1:length(values)) {
      value = values[i]
      code = codes[i]
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",code))
      message("mapping ",valueCol," to ",renameTo,", value ",value)
      df = df %>% mutate(!!renameTo := ifelse(!!valueCol == 1 & is.na(!!renameTo),value,!!renameTo))
      # no need to deal with ___na columns etc as columns start with NA. no way to tell missing from present but NA.
      # hide original ___1, ___2, etc columns
      df = df %>% rename(!!(paste0(".",valueCol)) := (!!valueCol))
      #TODO: validation checking?
    }
    
    # hide original ___na columns
    if(hasNa) df = df %>% rename(!!(paste0(".",naCol)) := (!!naCol))
    
    return(df)
    
  })
}

lrtd_normalise = function(lrtd_data = load_data("AvonCAPLRTDCentralDa")) {
  ethn = readr::read_csv(here::here("input/Ethnicity Data.csv"))
  # With the data from bristol the year and 
  lrtd_data2 = lrtd_data %>% left_join(ethn, by="record_number") %>%
    # the years are sometimes missing when 
    mutate(year = case_when(
      !is.na(year) ~ year,
      year == "y1" & week_number>30 ~ 2020, 
      year == "y1" & week_number<=30 ~ 2021,
      year == "y2" & week_number>30 ~ 2021, 
      year == "y2" & week_number<=30 ~ 2022,
      year == "y3" & week_number>30 ~ 2022, 
      year == "y3" & week_number<=30 ~ 2023,
      TRUE ~ NA_real_
    )) %>%
    mutate(study_week = (year-2020)*52+week_number)
  # These variables get picked up by the additional mappings in lrtd_mappings:
  lrtd_norm = lrtd_data2 %>% normalise_data()
  
  v = lrtd_norm %>% get_value_sets()
  
  lrtd_norm = lrtd_norm %>% mutate(
    # this is a study specific variable.
    admission.study_week_start_date = start_date_of_week(admission.study_week)
  )
  
  return(lrtd_norm)
}

## Column naming ----

.column_names = list(
  demog.imd_decile = "IMD (decile)",
  genomic.variant_inferred = "Variant",
  admission.curb_65_severity_score = "CURB65 score",
  demog.age = "Age",
  admission.news2_score = "NEWS2 score",
  qcovid2.hazard_ratio = "QCovid2 HR",
  qcovid2.log_hazard = "QCovid2 log hazard",
  admission.charlson_comorbidity_index = "CCI",
  admission.cci_category = "CCI category",
  admission.covid_pcr_result = "PCR result",
  day_7.WHO_clinical_progression = "WHO Outcome score",
  day_7.max_o2_level = "Max FiO\u2082",
  day_7.length_of_stay = "Length of Stay",
  demog.pcr_positive_by_age = "PCR positives (by age)",
  demog.age_eligible = "Age eligible for PneumoVax",
  admission.presentation_3_class = "aLTRD presentation"
)

# just a mapping function to get a readable label from the named variable.
# TODO: might be better to use the labelled package for this.
readable_label = function(columnVar, colNames = .column_names) {
  columnVar = ensym(columnVar)
  tmp = as_label(columnVar)
  if (!is.null(colNames[[tmp]])) {
    return(colNames[[tmp]])
  }
  tmp = tmp %>% stringr::str_remove("[a-zA-Z0-9_]+\\.") %>% stringr::str_replace_all("_"," ")
  if (tmp == toupper(tmp)) return(tmp)
  if (stringr::str_length(tmp)<=4) return(toupper(tmp))
  return(tmp %>% stringr::str_to_sentence())
}

# 
readable_label_mapping = function(columnVars, colNames = .column_names) {
  tmp = unname(columnVars) %>% sapply(readable_label, colNames=colNames)
  if(is.list(columnVars)) {
    names(tmp) = unname(columnVars) %>% sapply(as_label)
  } else {
    names(tmp) = unname(columnVars)
  }
  return(tmp)
}

## Study dates and weeks ----

study_week = function(dates) {
  return(as.integer(as.Date(dates)-as.Date("2019-12-30")) %/% 7)
}

start_date_of_week = function(study_week) {
  return(as.Date("2019-12-30")+study_week*7)
}

## Exclusions ----

standard_exclusions = function(avoncap_df) {
  tmp3 = avoncap_df
  v = tmp3 %>% get_value_sets()
  reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))
  
  # Self documenting exclusions
  tmp3 = tmp3 %>% 
    dtrackr::exclude_all(
      diagnosis.clinical_or_radiological_LRTI_or_pneumonia==v$diagnosis.clinical_or_radiological_LRTI_or_pneumonia$no & diagnosis.qualifying_symptoms_signs < 2 ~ "{.excluded} with fewer than 2 symptoms and not proven LRTI / pneumonia",
      demog.age<18 ~ "{.excluded} under 18 on admission",
      vaccination.first_dose_brand == v$vaccination.first_dose_brand$Pfizer & vaccination.first_dose_date < "2020-12-08" ~ "{.excluded} with first pfizer before 8/12/2020",
      vaccination.first_dose_brand == v$vaccination.first_dose_brand$AZ & vaccination.first_dose_date < "2021-01-04" ~ "{.excluded} with first AZ before 4/1/2021",
      admission.duration_symptoms > 10 ~ "{.excluded} symptomatic >10 days before admission",
      admission.date > reproduce_at-7 ~ "{.excluded} with admission after {format(reproduce_at-7, '%d/%m/%Y')}",
      admission.episode > 1 ~ "{.excluded} repeat admissions",
      .stage = "standard exclusions"
    )
  return(tmp3)
}


source(here::here("avoncap-mappings.R"))
source(here::here("avoncap-augment.R"))



