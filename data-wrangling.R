# Import AVONCAP data and cleanse into a tidy data format
# assuming there will be changes to the format we would like to be able to make
# the import reverse compatible to previous versions of the data.

here::i_am("data-wrangling.R")

## File handling functions ----

all_files = function() {
  files = fs::dir_ls(here::here("input")) %>% stringr::str_subset(".+\\.csv")
  info = fs::file_info(files) %>% mutate(filename = path %>% fs::path_ext_remove() %>% fs::path_file() %>% stringr::str_extract("[a-zA-Z\\s]+"))
  info %>% 
    mutate(
      modification_date = as.Date(modification_time),
      filename_date = path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_extract("\\d+") %>% stringr::str_replace("^(\\d{4})(\\d{2})$","\\120\\2") %>% as.Date(format="%d%m%Y"),
      version = (path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_match("_(.*?)_"))[,2],
      date = pmin(modification_date,filename_date,na.rm = TRUE)
    ) %>%
    select(filename, path, date, version)
}

# detecting errors when merging dataframes ----

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



#' Minimal function to load the AvonCAP data 
#' 
#' without making a whole lot of assumptions about where the data resides.
#' This does not check data integrity. It will probably complain if the csv files have different 
#' column names but I don't guarantee it. It uses readr::read_csv
#' 
#' Looking at this it will all need to be refactored but hopefully this is enough for the time being
#'
#' @param files a vector of full file names for all the data you want to load e.g. c("C:/files/DeltaVE_2021","C:/files/DeltaVE_2022")
#' @param versions a vector of version data for each file as a string e.g. c("y1","y2")
#'
#' @return a full data frame of whatever was in the files plus a version column (usually y1 etc), and a file column (with the filename).
minimal_loader = function(files, versions) {
  
  maxDate = file.info(files) %>% pull(ctime) %>% max() %>% as.Date()
  
  tmp = tibble(version = versions, file = files)
  tmp %>% 
    mutate(csv = purrr::map(path, ~ readr::read_csv(.x, na = c("","NA","na","N/A","UNK"), show_col_types = FALSE))) %>%
    unnest(csv)
  
  attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
  attr(tmp,"date") = unique(maxDate)
}



load_data = function(filename = "DeltaVE", reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date())), merge = TRUE) {
  if(reproduce_at != Sys.Date()) warning("REPRODUCING RESULTS FROM: ",reproduce_at, ", to disable this set options(reproduce.at=NULL)")
  tmp = filename
  # decide whether files are trust by trust or a single combined file
  tmp_files = all_files() %>% 
    filter(filename == tmp & date <= reproduce_at) %>% 
    group_by(version) %>% 
    filter(date == max(date)) %>% 
    # is this file a single file on a particular date with no version, or a versioned set of files (originally this was trust specific but also happens when it is year specific?
    mutate(byTrust = !is.na(version)) %>% 
    # get the most recent file or set of files if it is versioned.
    group_by(byTrust) %>% 
    mutate(latest_date = max(date)) %>% ungroup() %>%
    filter(latest_date == max(latest_date)) %>%
    ungroup() %>%
    select(-byTrust)
  files = tmp_files %>% pull(path)
  if (length(files)>1 & length(unique(tmp_files$date)) > 1) {
    warning("The most recent versions of the inputs have different effective dates: ",filename," i.e. ", paste0(files,collapse = ";"))
    warning("This could mean the files are out of sync.")
  }
  data = tmp_files %>% 
    mutate(csv = purrr::map(path, ~ readr::read_csv(.x, na = c("","NA","na","N/A","UNK"), show_col_types = FALSE)))
  data2 = data %>% 
    mutate(structure = .detect_structure(csv), file = fs::path_file(path)) %>%
    mutate(empty = .detect_empty(structure)) %>%
    mutate(mismatches = .detect_mismatch(structure)) %>%
    select(-path,-filename)
  
  col_suppress = unique(c(unlist(data2$mismatches)))
  # Lose empty columns which will be incorrect type
  data2 = data2 %>% mutate(csv = purrr::map2(csv, empty, ~ .x %>% select(-any_of(.y))))
  
  if(length(col_suppress) > 0) {
    warning("INCONSISTENT COLUMN(S) IN FILES: ",paste0(col_suppress,collapse=";"))
  }
  
  if (!merge) {
    return(data2 %>% select(version,file,csv))
  }
  
  tmp = data2 %>% 
    # Lose conflicting data type columns
    mutate(csv = purrr::map2(csv, mismatches, ~ .x %>% select(-any_of(.y)))) %>% 
    select(version,file,csv) %>% 
    unnest(csv)
  
  attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
  attr(tmp,"date") = unique(tmp_files$latest_date)
  return(tmp)
}

## Data normalisation infrastructure ----


normalise_data = function(rawData, remove_mapped = TRUE, remove_unmapped = TRUE, mappings=.mappings, messages = c("files: {files}","{.total} rows","analysis from: {reproduce_at}","files from: {date}")) {
  
  reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))
  files = attr(rawData,"file")
  date = attr(rawData,"date")
  tmp = rawData
  
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
  
  return(tmp %>% track(.messages = messages) %>% capture_exclusions())
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

## Standard data manipulation post load ----

augment_data = function(avoncap_original) {
  
  tmp2 = avoncap_original
  v = tmp2 %>% get_value_sets()
  
  # pick up older format for admission swab ----
  if("diagnosis.admission_swab_old" %in% colnames(tmp2)) {
    tmp2 = tmp2 %>% mutate(
      diagnosis.admission_swab = ifelse(is.na(diagnosis.admission_swab), diagnosis.admission_swab_old, diagnosis.admission_swab)
    )
  }
  
  # SOC 4 presentation categories ----
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      diagnosis.pneumonia = ifelse(
        diagnosis.SOC_CAP_clinically_confirmed == v$diagnosis.SOC_CAP_clinically_confirmed$yes |
        diagnosis.SOC_CAP_radiologically_confirmed == v$diagnosis.SOC_CAP_radiologically_confirmed$yes |
        diagnosis.SOC_CAP_no_radiology == v$diagnosis.SOC_CAP_no_radiology$yes |
        diagnosis.SOC_Empyema_or_abscess == v$diagnosis.SOC_Empyema_or_abscess$yes |
        (!is.na(admission.cxr_pneumonia) & admission.cxr_pneumonia == v$admission.cxr_pneumonia$yes),
        "yes","no") %>% factor(levels = c("no","yes")),
      diagnosis.LRTI = ifelse(
        #TODO: clarify whether preventing overlap between pneumonia and LRTD is desirable here or should be explicit exclusion later (and tracked for data quality).
        diagnosis.pneumonia == "no" & 
        diagnosis.SOC_LRTI == v$diagnosis.SOC_LRTI$yes,
        "yes","no") %>% factor(levels = c("no","yes")),
      diagnosis.exacerbation_of_chronic_respiratory_disease = ifelse(
        diagnosis.SOC_exacerbation_COPD == v$diagnosis.SOC_exacerbation_COPD$yes |
        diagnosis.SOC_exacerbation_non_COPD == v$diagnosis.SOC_exacerbation_non_COPD$yes,
        "yes","no") %>% factor(levels = c("no","yes")),
      diagnosis.heart_failure = ifelse(
        diagnosis.SOC_congestive_heart_failure == v$diagnosis.SOC_congestive_heart_failure$yes,
        "yes","no") %>% factor(levels = c("no","yes"))
      )
  }, error = function(e) warning("could not compute SOC diagnoses: This is normal for the hospital AvonCap data set: ", e$message))
   
  # Infective cause & covid status ---- 
  tmp2 = tryCatch({
    tmp2 %>% mutate(
      admission.infective_cause = case_when(
        diagnosis.pneumonia == "yes" ~ "Infective",
        diagnosis.LRTI == "yes" ~ "Infective",
        diagnosis.SOC_non_infectious_process == v$diagnosis.SOC_non_infectious_process$yes ~ "Non-infective",
        diagnosis.SOC_non_LRTI == v$diagnosis.SOC_non_LRTI$yes ~ "Non-infective",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed` ~ "Infective",
        TRUE ~ "Non-infective"
      ) %>% factor(levels = c("Non-infective","Infective"))
    )

  }, error = function(e) {
    warning("falling back to diagnosis.standard_of_care_COVID_diagnosis")
    tmp2 %>% mutate(
      admission.infective_cause = case_when(
        diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` ~ "Infective",
        diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$Pneumonia ~ "Infective",
        diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$LRTI ~ "Infective",
        TRUE ~ "Non-infective"
      ) %>% factor(levels = c("Non-infective","Infective"))
    )
  })
  
  
  tmp2 = tryCatch({
    # This part works in the hospital data set (lrti incidence)
    tmp2 %>% mutate(
      admission.covid_pcr_result = case_when(
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed` ~ "SARS-CoV-2 PCR positive",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - patient reported test` ~ "SARS-CoV-2 PCR negative",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - clinical diagnosis (but negative test)` ~ "SARS-CoV-2 PCR negative",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - negative test, unlikely COVID-19 disease` ~ "SARS-CoV-2 PCR negative",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`No test performed` ~ NA_character_,
        TRUE ~ NA_character_
      ) %>% factor(levels = c("SARS-CoV-2 PCR positive","SARS-CoV-2 PCR negative")),
    )
  }, error = function(e) {
      message("falling back to admission_swab data point for covid status")
      tmp2 %>% mutate(
        admission.covid_pcr_result = case_when(
          diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` ~ "SARS-CoV-2 PCR positive",
          diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 negative` ~ "SARS-CoV-2 PCR negative",
          TRUE ~ NA_character_) %>% factor(levels = c("SARS-CoV-2 PCR positive","SARS-CoV-2 PCR negative")),
      )
  })
  
  tryCatch({
    # Consent given ----
    tmp2 = tmp2 %>% mutate(
      admin.consent_withheld = case_when(
        admin.consented == v$admin.consented$`Declined consent` ~ "yes",
        admin.pp_consented == v$admin.pp_consented$`Declined consent` ~ "yes",
        admin.withdrawal == v$admin.withdrawal$yes ~ "yes",
        TRUE ~ "no") %>% factor(levels = c("no","yes"))
    ) %>%
    mutate(
      admission.infective_cause = ifelse(admin.consent_withheld == "yes", NA, as.character(admission.infective_cause)) %>% factor(levels=levels(admission.infective_cause)),
      admission.covid_pcr_result = ifelse(admin.consent_withheld == "yes", NA, as.character(admission.covid_pcr_result)) %>% factor(levels=levels(admission.covid_pcr_result))
    ) 
  }, error = function(e) warning("could not compute consent: This is normal for the hospital AvonCap data set: ", e$message))
    
    
    
  
  # determine covid patients versus controls ----
  tmp2 = tmp2 %>% 
    mutate(
      cohort = if_else(
        # Original logic here depends on items that are not in current data dump
        # e.g. c19_adm_swab
        # (
        #   diagnosis.COVID_positive == v$diagnosis.COVID_positive$yes &
        #   diagnosis.COVID_negative == v$diagnosis.COVID_negative$no &
        #   diagnosis.no_COVID_test == v$no_COVID_test$no
        # )
        # OR c19_adm_status
        # (
        #   ( 
        #     diagnosis.COVID_laboratory_confirmed == v$diagnosis.COVID_laboratory_confirmed$yes |
        #     diagnosis.COVID_patient_reported_test == v$diagnosis.COVID_patient_reported_test$yes |
        #     diagnosis.COVID_clinical_diagnosis == v$diagnosis.COVID_clinical_diagnosis$yes
        #   ) &
        #   diagnosis.not_COVID_negative_test !=  v$diagnosis.not_COVID_negative_test$yes &
        #   diagnosis.not_tested_for_COVID != v$diagnosis.not_tested_for_COVID$yes
        # )
        # I don't think this is an adequate alternative:
        # is.na(diagnosis.clinical_or_radiological_LRTI_or_pneumonia),
          !is.na(diagnosis.meets_case_control_criteria) & 
          diagnosis.meets_case_control_criteria == v$diagnosis.meets_case_control_criteria$yes &
          diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive`,
        "case","control") %>% factor(levels = c("case","control"))
    )
  
    # determine if infection was possibly nosocomial ----
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      diagnosis.infection_context = #.maybe(
          case_when(
            cohort == "control" ~ NA_character_,
            is.na(diagnosis.first_COVID_positive_swab_date) ~ "Unknown",
            diagnosis.first_COVID_positive_swab_date < admission.date ~ "Community",
            diagnosis.first_COVID_positive_swab_date < admission.date+7 ~ "Probable community",
            diagnosis.first_COVID_positive_swab_date < admission.date+28 ~ "Possible nosocomial",
            TRUE ~ NA_character_
          ) %>% factor(levels = c("Community","Probable community","Possible nosocomial","Unknown"))
       # )
    )
  }, error = function(e) warning("could not compute infection context: ", e$message))
  
  # infer variant status ----
  tryCatch({
    
    maxDelta = as.Date("2022-02-07") # tmp2 %>% filter(genomic.variant == "Delta") %>% summarise(max = max(admission.date)) %>% pull(max)
    minOmicron = as.Date("2021-11-07") # tmp2 %>% filter(genomic.variant == "Omicron") %>% summarise(min = min(admission.date)) %>% pull(min)
    # message("Inferring Delta for cases before ",minOmicron," and inferring Omicron for cases after ",maxDelta)
    
    tmp2 = tmp2 %>% mutate(
      genomic.variant_inferred = case_when(
        cohort == "control" ~ NA_character_,
        !is.na(genomic.variant) ~ as.character(genomic.variant),
        admission.date > maxDelta ~ "Omicron",
        admission.date >= minOmicron ~ NA_character_,
        admission.date > "2021-06-01" ~ "Delta",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Delta","Omicron"))
    )
  }, error = function(e) warning("could not infer variant status from dates: ", e$message))
  
  # Compute symptom onset dates ----
  #TODO NHS Redcap only
  tryCatch({
    # Use symptom onset dates rather than admission dates
    tmp2 = tmp2 %>% 
      mutate(
        symptom_onset.date_of_symptoms = admission.date - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_first_vaccine_dose = admission.time_since_first_vaccine_dose - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_second_vaccine_dose = admission.time_since_second_vaccine_dose - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_third_vaccine_dose = admission.time_since_third_vaccine_dose - admission.duration_symptoms
      ) 
  }, error = function(e) warning("could not compute symptom onset dates: ", e$message))
  
  # calculate various time intervals ----
  tryCatch({
    #TODO NHS Redcap only
    tmp2 = tmp2 %>% 
      mutate(
        admission.week_no = round(time_length(interval(min(admission.date), admission.date), "weeks")),
        vaccination.dose_interval = admission.time_since_first_vaccine_dose - admission.time_since_second_vaccine_dose,
        vaccination.booster_interval = admission.time_since_second_vaccine_dose - admission.time_since_third_vaccine_dose
      )
  }, error = function(e) warning("could not compute admission intervals: ", e$message))
  
  # TODO: 
  tryCatch({
  # # establish vaccine protection / immune status on admission ----
  # # TODO: do we have any clear defined categories for these?
  tmp2 = tmp2 %>% mutate(
      vaccination.protection = #.maybe(
        case_when(
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) & 
            admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3rd dose 7d+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2nd dose 7d+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1st dose 14d+",
          admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Recovered",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose < 0+.na.default(admission.duration_symptoms,0) ~ "Before 1st dose",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose < 14+.na.default(admission.duration_symptoms,0) ~ "1st dose 0-13d",
          TRUE ~ "Unvaccinated"
        ) %>% ordered(c("Unvaccinated","Recovered","Before 1st dose","1st dose 0-13d","1st dose 14d+","2nd dose 7d+","3rd dose 7d+"))
     # )
  )
  }, error = function(e) warning("could not compute vaccination protection status: ", e$message))
    
  tryCatch({
    #TODO NHS Redcap only
    tmp2 = tmp2 %>% mutate(
      vaccination.vaccination = #.maybe(
          case_when(
          # vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
          #   symptom_onset.time_of_symptoms_since_second_vaccine_dose >= 7 & admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1 dose",
          TRUE ~ "Unvaccinated"
        ) %>% ordered(c("Unvaccinated","1 dose","2 doses","3 doses"))
      #)
    )
  }, error = function(e) warning("could not compute simple covid vaccination status: ", e$message)) 
  
  tryCatch({
    #TODO NHS Redcap only
    tmp2 = tmp2 %>% mutate(
      vaccination.immune_exposure = #.maybe(
        case_when(
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) & 
            admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "3+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) & 
            admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "2",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2",
          admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "1",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received & 
            admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1",
          TRUE ~ "None"
        ) %>% ordered(c("None","1","2","3+"))
      #)
    )
  }, error = function(e) warning("could not compute immune exposure status: ", e$message))
  
  # Vaccine brand combinations ----
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      vaccination.brand_combination = paste(
        case_when(
          vaccination.first_dose_brand == v$vaccination.first_dose_brand$Pfizer ~ "Pf",
          vaccination.first_dose_brand == v$vaccination.first_dose_brand$AZ ~ "AZ",
          vaccination.first_dose_brand == v$vaccination.first_dose_brand$unknown ~ "??",
          vaccination.first_dose_brand == v$vaccination.first_dose_brand$Moderna ~ "Mo",
          vaccination.first_dose_brand == v$vaccination.first_dose_brand$Janssen ~ "Ja",
          TRUE ~ "xx"
        ),
        case_when(
          vaccination.second_dose_brand == v$vaccination.second_dose_brand$Pfizer ~ "Pf",
          vaccination.second_dose_brand == v$vaccination.second_dose_brand$AZ ~ "AZ",
          vaccination.second_dose_brand == v$vaccination.second_dose_brand$unknown ~ "??",
          vaccination.second_dose_brand == v$vaccination.second_dose_brand$Moderna ~ "Mo",
          vaccination.second_dose_brand == v$vaccination.second_dose_brand$Janssen ~ "Ja",
          TRUE ~ "xx"
        ),
        case_when(
          vaccination.third_dose_brand == v$vaccination.third_dose_brand$Pfizer ~ "Pf",
          vaccination.third_dose_brand == v$vaccination.third_dose_brand$AZ ~ "AZ",
          vaccination.third_dose_brand == v$vaccination.third_dose_brand$unknown ~ "??",
          vaccination.third_dose_brand == v$vaccination.third_dose_brand$Moderna ~ "Mo",
          vaccination.third_dose_brand == v$vaccination.third_dose_brand$Janssen ~ "Ja",
          TRUE ~ "xx"
        ),
        sep="-"
      ) %>% forcats::as_factor()
    )
  }, error = function(e) warning("could not compute vaccination dosing regimen status: ", e$message))
  
  # rationalise comorbidities ----
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      comorbid.diabetes_type = case_when(
        comorbid.diabetes == v$comorbid.diabetes$None ~ "None",
        comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` ~ "Type 1",
        comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` ~ "Type 1",
        comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` ~ "Type 2",
        comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` ~ "Type 2",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("None","Type 1","Type 2")),
      comorbid.solid_cancer_present = case_when(
        comorbid.solid_cancer == v$comorbid.solid_cancer$None ~ "no",
        comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
        comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("no","yes")),
      comorbid.haemotological_cancer_present = case_when(
        comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
        comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
        comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
        TRUE ~ "no"
      ) %>% factor(levels = c("no","yes")),
      comorbid.any_cancer_present = case_when(
        comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
        comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
        comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
        comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
        comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
        TRUE ~ "no"
      ) %>% factor(levels = c("no","yes"))
    )
  }, error = function(e) warning("could not rationalise comorbidities: ", e$message))
  
  # determine high pneumococcal risk group ----
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      admission.frailty_score = cut(admission.rockwood_score, breaks=c(0,5,Inf), labels=c("0-4","5-9"),ordered_result = TRUE),
      admission.pneumococcal_risk_group = ifelse(
        demog.age >= 65 |
          comorbid.other_pneumococcal_risks == v$comorbid.other_pneumococcal_risks$yes |
          comorbid.copd == v$comorbid.copd$yes |
          comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes |
          comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes |
          comorbid.hypertension == v$comorbid.hypertension$yes |
          comorbid.ccf == v$comorbid.ccf$yes |
          comorbid.ihd == v$comorbid.ihd$yes |
          comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` |
          comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease without failure` |
          comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` |
          comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes & comorbid.asthma == v$comorbid.asthma$yes |
          admission.on_immunosuppression == v$admission.on_immunosuppression$yes
        ,
        "yes","no") %>% factor(levels=c("no","yes"))
    )
  }, error=function(e) warning("Could not calculate pneumococcal risk group", e$message))
    
  # determine WHO outcome  ----
  # 4: Hospitalised; no oxygen therapy*
  # 5: Hospitalised; oxygen by mask or nasal prongs
  # 6: Hospitalised; oxygen by NIV or high flow
  # 7: Intubation and mechanical ventilation, pO2/FiO2 ≥150 or SpO2/FiO2 ≥200
  # 8: Mechanical ventilation pO2/FIO2 <150 (SpO2/FiO2 <200) or vasopressors
  # 9: Mechanical ventilation pO2/FiO2 <150 and vasopressors, dialysis, or ECMO
  # 10: Dead
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      day_7.WHO_clinical_progression = case_when(
        cohort == "control" ~ "score 0",
        day_7.death == v$day_7.death$yes ~ "score 10",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT & day_7.ionotropes_needed == v$day_7.ionotropes_needed$yes   ~ "score 9",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT ~ "score 7-8",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$`NIV (CPAP/BiPAP)` ~ "score 6",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$HFNO ~ "score 6",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$Oxygen ~ "score 5",
        day_7.max_o2_level == v$day_7.max_o2_level$`24-28%` ~ "score 5",
        day_7.max_o2_level == v$day_7.max_o2_level$`room air` ~ "score 4",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$None ~ "score 4",
        TRUE ~ NA_character_
        
      ) %>% ordered(levels = c("score 0","score 4","score 5","score 6","score 7-8","score 9","score 10")) 
    )
  }, error=function(e) warning("Could not calculate WHO outcome group", e$message))
  
  # determine salami slice categories ----
  
  tryCatch({
    tmp2 = tmp2 %>% mutate(
      admission.is_covid = case_when(
        admission.covid_pcr_result == "SARS-CoV-2 PCR positive" ~ "Confirmed SARS-CoV-2",
        TRUE ~ "No evidence SARS-CoV-2"
      ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2")),
      admission.category = case_when(
        admission.is_covid == "Confirmed SARS-CoV-2" ~ "Confirmed SARS-CoV-2",
        admission.is_covid == "No evidence SARS-CoV-2" & admission.infective_cause == "Infective" ~ "No evidence SARS-CoV-2",
        admission.infective_cause == "Non-infective" ~ "Non-infective",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2","Non-infective"))
    )
  }, error=function(e) warning("Could not calculate main salami slice categories", e$message))    
  
  tmp2 = tryCatch({
    tmp2  %>%
      mutate(
        admission.presentation_3_class = case_when(
          diagnosis.pneumonia == "yes" ~ "Pneumonia",
          diagnosis.LRTI == "yes" ~ "NP-LRTI",
          admission.infective_cause == "Non-infective" ~ "Non-infective",
          TRUE ~ NA_character_
        ) %>% factor(levels = c("Pneumonia","NP-LRTI","Non-infective")),
      ) 
  }, error=function(e) {
    warning("falling back to diagnosis.standard_of_care_COVID_diagnosis")
    tmp2 %>%
      mutate(
        admission.presentation_3_class = case_when(
          diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$Pneumonia ~ "Pneumonia",
          diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$LRTI ~ "NP-LRTI",
          is.na(diagnosis.standard_of_care_COVID_diagnosis) ~ NA_character_,
          TRUE ~ "Non-infective"
        ) %>% factor(levels = c("Pneumonia","NP-LRTI","Non-infective"))
      )
  })
  
  # demographics categorisation (defaults may be overridden in a specific analysis) ----
  tmp2 = tmp2 %>%
    mutate(
      demog.age_category = cut(demog.age,breaks = c(0,35,50,65,75,85,Inf), labels = c("18-34","35-49","50-64","65-74","75-84","85+"), include.lowest = FALSE, ordered_result = TRUE),
      demog.age_eligible = cut(demog.age,breaks = c(0,65,Inf), labels = c("18-64","65+"),ordered_result = TRUE),
      admission.cci_category = cut(admission.charlson_comorbidity_index, breaks = c(-Inf,0,2,4,Inf), labels=c("none (0)","mild (1-2)","moderate (3-4)","severe (5+)"), include.lowest = FALSE, ordered_result = TRUE)
    )
    
  
  # determine salami slice outcomes ----
  tryCatch({
    tmp2 = tmp2 %>% 
      mutate(
        # outcome.length_of_stay
        # outcome.icu_duration
        outcome.icu_admission = case_when(
          outcome.icu_duration > 0 ~ "confirmed",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded")),
        outcome.death_during_follow_up = case_when(
          outcome.inpatient_death_days > 0 ~ "confirmed",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded"))
      )
  }, error=function(e) warning("Could not calculate aLTRD (salami slice) outcomes", e$message))
  
  # omicron severe disease outcomes ----
  tryCatch({
    tmp2 = tmp2 %>% 
      mutate(
        # MAX O2 within first 7 days
        day_7.max_o2_gt_28 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`24-28%`,"28% and under","over 28%") %>% ordered(c("28% and under","over 28%")),
        day_7.max_o2_gt_35 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`30-35%`,"35% and under","over 35%") %>% ordered(c("35% and under","over 35%")),
        day_7.max_o2_gt_50 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`50%`,"50% and under","over 50%") %>% ordered(c("50% and under","over 50%")),
        # WHO outcome score within first 7 days
        day_7.WHO_score_gt_6 = ifelse(day_7.WHO_clinical_progression > "score 6","WHO score 7-10","WHO score 4-6") %>% ordered(c("WHO score 4-6","WHO score 7-10")),
        day_7.WHO_score_gt_5 = ifelse(day_7.WHO_clinical_progression > "score 5","WHO score 6-10","WHO score 4-5") %>% ordered(c("WHO score 4-5","WHO score 6-10")),
        # day_7.any_ICU = ifelse(day_7.icu_length_of_stay > v$day_7.icu_length_of_stay$`0 day`,"ICU admission","No ICU admission") %>% ordered(c("No ICU admission","ICU admission")),
        # LOS within first 7 days
        day_7.los_gt_3 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`3 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 3),"LOS>3 days","LOS<=3 days") %>% ordered(c("LOS<=3 days","LOS>3 days")),
        day_7.los_gt_5 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`5 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 5),"LOS>5 days","LOS<=5 days") %>% ordered(c("LOS<=5 days","LOS>5 days")),
        day_7.los_gt_7 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`7 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 7),"LOS>7 days","LOS<=7 days") %>% ordered(c("LOS<=7 days","LOS>7 days"))
      )
  }, error=function(e) stop("Could not calculate omicron severity outcomes", e$message))

  
  # hospital of admission ----
  tryCatch({
    tmp2 = tmp2 %>% 
      mutate(
        admin.hospital = case_when(
          stringr::str_starts(admin.record_number,"N") ~ "Hospital 1",
          TRUE ~ "Hospital 2"
        ) %>% factor(levels=c("Hospital 1","Hospital 2"))
      )
  }, error=function(e) warning("Could not determine hospital of admission", e$message))
  
  return(tmp2)
}

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




