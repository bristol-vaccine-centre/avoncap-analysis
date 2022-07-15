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
  
  tmp2 = suppressMessages(
    tmp2 %>% dtrackr::pause() %>% 
    group_by(admin.record_number, admission.date) %>% 
    mutate(admin.duplicate = ifelse(row_number()>1, "yes", "no") %>% factor(levels = c("no","yes"))) %>% 
    ungroup() %>% dtrackr::resume()
  )
  
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
    
    # Bristol specific data is here
    # https://covid19.sanger.ac.uk/lineages/raw?date=2021-07-24&area=E06000023&lineageView=1&lineages=A%2CB%2CB.1.1.7%2CB.1.617.2%2CB.1.1.529&colours=7%2C3%2C1%2C6%2C2
    # https://covid19.sanger.ac.uk/ee0c813f-1706-4fee-a69f-0d642aa4c5a7
    
    minAlpha = as.Date("2020-12-05")
    
    maxWuhan = as.Date("2021-02-13")
    
    minDelta = as.Date("2021-05-15")
    
    # maxAlpha = as.Date("2021-06-26") # officially according to sanger but there were very low levels of Alpha 
    maxAlpha = as.Date("2021-06-01") # unofficially this cutoff was used in the Delta Omicron paper.
    
    # minOmicron = as.Date("2021-11-27") # according to sanger
    minOmicron = as.Date("2021-11-07") # according to in hospital data
    # i.e. tmp2 %>% filter(genomic.variant == "Omicron") %>% summarise(min = min(admission.date)) %>% pull(min)
    
    # maxDelta = as.Date("2022-01-15") # according to sanger
    maxDelta = as.Date("2022-02-07") # according to in hospital results
    # i.e. tmp2 %>% filter(genomic.variant == "Delta") %>% summarise(max = max(admission.date)) %>% pull(max)
    
    
    # message("Inferring Delta for cases before ",minOmicron," and inferring Omicron for cases after ",maxDelta)
    tmp2 = tmp2 %>% mutate(
      genomic.variant_inferred = case_when(
        #cohort == "control" ~ NA_character_,
        #!is.na(genomic.variant) ~ as.character(genomic.variant),
        !is.na(genomic.variant) & genomic.variant != "unknown" ~ as.character(genomic.variant),
        admission.date < minAlpha ~ "Pre-alpha",
        admission.date > maxWuhan & admission.date < minDelta ~ "Alpha",
        admission.date > maxAlpha & admission.date < minOmicron ~ "Delta",
        admission.date > maxDelta ~ "Omicron",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Pre-alpha","Alpha","Delta","Omicron"))
    ) %>% dtrackr::comment(.headline = "Infering genomic variant",.messages = c(
      "If sequencing not available, we assume:",
      "Pre-alpha before {fdmy(minAlpha)}",
      "Alpha between {fdmy(maxWuhan)} and {fdmy(minDelta)}",
      "Delta between {fdmy(maxAlpha)} and {fdmy(minOmicron)}",
      "Omicron from {fdmy(maxDelta)} to present"
    ), .tag = "variants-inference")
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
      demog.age_eligible = cut(demog.age,breaks = c(0,65,Inf), labels = c("18-64","65+"),ordered_result = TRUE)
    )
      
      
  tmp2 = tmp2 %>%
    mutate(
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
  
  return(tmp2)
}

