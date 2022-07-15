here::i_am("lrtd-incidence/lrtd-incidence-data.R")
source(here::here("lrtd-incidence/lrtd-mappings.R"))



# returns a tracked data frame
lrtd_study_data = function(lrtd_norm = lrtd_normalise(), study_start = NULL, study_end = Sys.Date()) {
  
  v = lrtd_norm %>% get_value_sets()
  
  lrtd_aug = lrtd_norm %>% augment_data() %>% mutate(
    # this is a study specific variable.
    # add in dates ----
    admission.study_week_start_date = start_date_of_week(admission.study_week),
    admin.hospital = recode_factor(admin.hospital, NBT = "Hospital 1", BRI = "Hospital 2"),
    demog.pcr_positive_by_age = if_else(diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed`, demog.age_category, ordered(NA)),
  )
  
  ## Fix start and end dates of the study
  if (is.null(study_start)) {
    start_week = min(lrtd_aug$admission.study_week,na.rm = TRUE)
  } else {
    start_week = study_week(as.Date(study_start))
  }
  end_week = study_week(as.Date(study_end))
  
  tmp = lrtd_aug %>% 
    filter(start_week <= admission.study_week & admission.study_week < end_week) %>%
    dtrackr::p_clear() %>%
    capture_exclusions() %>%
    comment(c(
      "135014 adult admissions", #TODO: this was true for SALAMI only
      "between {start_date_of_week(start_week)} and {start_date_of_week(end_week)}"
    )) %>%
    comment(c(
      "121773 excluded:", #TODO: this was true for SALAMI only
      "No respiratory signs/symptoms of respiratory disease",
      "No confirmed clinical/ radiological respiratory disease diagnosis"),
      .type="exclusion"
    ) %>%
    comment(c(
      "19775 adult admissions",
      "with signs, symptoms, or diagnosis",
      "of respiratory disease")
    ) %>%
    comment(c(
      "{19775-.total} excluded:",
      "non aLRTD only diagnoses (eg urosepsis, PE, pneumothorax)"),
      .type="exclusion"
    ) %>%
    comment(c(
      "{.total} adult aLTRD admissions"
    )) %>%
    exclude_all(
      admin.duplicate == v$admin.duplicate$yes ~ "{.excluded} excluded:\n Duplicate admissions for both sites",
    ) %>% 
    comment("{.count} qualifying\nadult aLTRD admissions") 
  
  
  tmp2 = tmp %>% 
    exclude_all(
      admin.consented == v$admin.consented$`Declined consent` | admin.pp_consented == v$admin.pp_consented$`Declined consent` ~ "{.excluded} declined consent",
      admin.withdrawal == v$admin.withdrawal$yes ~ "{.excluded} withdrew consent"
    ) %>% 
  group_by(admission.infective_cause, .messages="{.count} admissions\nwith consent") %>%
    status(
      pneumonias = sum(diagnosis.pneumonia == v$diagnosis.pneumonia$yes),
      lrtis = sum(diagnosis.LRTI == v$diagnosis.LRTI$yes),
      crdes = sum(diagnosis.exacerbation_of_chronic_respiratory_disease == v$diagnosis.exacerbation_of_chronic_respiratory_disease$yes),
      hfs = sum(diagnosis.heart_failure == v$diagnosis.heart_failure$yes),
      others = sum(
        diagnosis.pneumonia != v$diagnosis.pneumonia$yes & 
          diagnosis.LRTI != v$diagnosis.LRTI$yes & 
          diagnosis.exacerbation_of_chronic_respiratory_disease != v$diagnosis.exacerbation_of_chronic_respiratory_disease$yes & 
          diagnosis.heart_failure != v$diagnosis.heart_failure$yes),
      .headline = "{.strata}",
      .messages = c(
        "Total {.count} admissions:",
        "{pneumonias} with Pneumonia",
        "{lrtis} with NP-LRTI",
        "{crdes} with CRDE",
        "{hfs} with HF",
        "{others} with other presentations"
      )
    ) %>%
    # exclude_all(admission.infective_cause == "Non-infective" & admission.is_covid == "Confirmed SARS-CoV-2" ~ "{.excluded} incidental COVID\ndiscovered on admission") %>%
    group_by(admission.category, .add=TRUE, .messages="{.count} admissions") %>%
    status(
      pneumonias = sum(diagnosis.pneumonia == v$diagnosis.pneumonia$yes),
      lrtis = sum(diagnosis.LRTI == v$diagnosis.LRTI$yes),
      crdes = sum(diagnosis.exacerbation_of_chronic_respiratory_disease == v$diagnosis.exacerbation_of_chronic_respiratory_disease$yes),
      hfs = sum(diagnosis.heart_failure == v$diagnosis.heart_failure$yes),
      others = sum(
        diagnosis.pneumonia != v$diagnosis.pneumonia$yes & 
          diagnosis.LRTI != v$diagnosis.LRTI$yes & 
          diagnosis.exacerbation_of_chronic_respiratory_disease != v$diagnosis.exacerbation_of_chronic_respiratory_disease$yes & 
          diagnosis.heart_failure != v$diagnosis.heart_failure$yes),
      .headline = "{admission.category}",
      .messages = c(
        "Total {.count} admissions:",
        "{pneumonias} with Pneumonia",
        "{lrtis} with NP-LRTI",
        "{crdes} with CRDE",
        "{hfs} with HF",
        "{others} with other presentations"
      )
    ) %>%
    ungroup("{.total} adult aLTRD admissions") 
  
  return(tmp2)
}

