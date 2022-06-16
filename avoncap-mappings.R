.mappings = list(
  
  # TODO:
  # 1) "did_the_patient_have_respi" = .normalise_yesno, actually 4 options, not clear what they are
  # 2) change this to deal with explicit NA options in the drop downs otherwise we are missing explicit missing values 
  # in the dropdowns.
  "record_number" = .normalise_name(admin.record_number),
  "duplicate" = .normalise_yesno(admin.duplicate),
  "c19_diagnosis" = .normalise_list(
    diagnosis.standard_of_care_COVID_diagnosis, c("Pneumonia","LRTI","HF","other resp symptoms")),
  # "acute_illness" = .normalise_yesno(
  #   diagnosis.acute_illness
  # ), # acute illness will always be 1
  # "covid19" = .normalise_yesno(
  #   diagnosis.community_diagnosed_COVID_admission
  # ),
  # "current"= .normalise_yesno(
  #   diagnosis.current_COVID_episode
  # ), COVID19 and current are slight nonsense fields - that spit out what is, in essence, garbage
  "clinical_radio_diagnosis" = .normalise_yesno(
    diagnosis.clinical_or_radiological_LRTI_or_pneumonia
  ),
  # "c19_adm_swab" = .normalise_checkboxes(vars(
  #   diagnosis.COVID_positive,
  #   diagnosis.COVID_negative,
  #   diagnosis.no_COVID_test,
  # )), # So this field changed from checkboxes to a list.
  "c19_adm_swab" = .normalise_list(
    diagnosis.admission_swab,
    values = c("COVID-19 positive","COVID-19 negative","Indeterminate","Known community/recent positive","Not performed")
  ),
  # "c19_adm_status" = .normalise_checkboxes(vars(
  #   diagnosis.COVID_laboratory_confirmed,
  #   diagnosis.COVID_patient_reported_test,
  #   diagnosis.COVID_clinical_diagnosis,
  #   diagnosis.not_COVID_negative_test,
  #   diagnosis.not_tested_for_COVID
  # )), # c19_adm_status isnt being completed for this round of case control - as it was done so badly the first time around it was nonsense
  "c19_test_type" = .normalise_list(
    diagnosis.test_type,
    values = c("Lateral Flow Only","PCR Confirmed")
  ),
  "qualifying_symptoms_signs" = .normalise_name(
    diagnosis.qualifying_symptoms_signs),
  "cc_critieria" = .normalise_yesno(
    diagnosis.meets_case_control_criteria
  ),
  "cc_pos_date" = .normalise_date(
    diagnosis.first_COVID_positive_swab_date
  ),
  # "exclusion_criteria" = .normalise_yesno(
  #   diagnosis.meets_exclusion_criteria
  # ), # always 0
  
  #### Demographics ----
  "gender" = .normalise_list(
    demog.gender, c("Male","Female")),
  "age_at_admission" = .normalise_double(
    demog.age, limits=c(0,120)),
  "imd" = .normalise_name(
    demog.imd_decile),
  
  
  "smoking" = .normalise_list(
    demog.smoker, c("Non-smoker","Current","Ex-smoker","Unknown"),codes = c(3,1,2,4)),
  "ethnicity2" = .normalise_list(
    demog.ethnicity, c(
      "White British",
      "White other",
      "Mixed origin",
      "Black",
      "Asian",
      "Other race",
      "Unknown"
    )),
  "care_home" = .normalise_yesno(
    demog.care_home_resident),
  "drugs" = .normalise_checkboxes(vars(
    demog.no_drug_abuse,
    demog.alcohol_abuse,
    demog.ivdu_abuse,
    demog.marijuana_abuse,
    demog.other_inhaled_drug_abuse
  )),
  "alc_units" = .normalise_name(
    demog.units_of_alcohol
  ),
  
  
  #### Vaccination fields: ----
  # TODO: Third dose brand is currently messed up with inconsistently transposed brand and date information.
  "covid19_vax" = .normalise_list(
    vaccination.covid_vaccination, c("Not received","Received","Unknown"), codes=c(2,1,3)
  ),
  "covidvax_date" = .normalise_date(
    vaccination.first_dose_date),
  "covidvax_dose_2" = .normalise_date(
    vaccination.second_dose_date),
  "covidvax_dose_3" = .normalise_date(
    vaccination.second_dose_date),
  "brand_of_covid19_vaccinati" = .normalise_list(
    vaccination.first_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen")),
  "covid19vax_brand_2" = .normalise_list(
    vaccination.second_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen")),
  "covid19vax_brand_3" =  .normalise_list(
    vaccination.third_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen")),
  
  # Time since vaccination
  "c19vaxd1_adm" = .normalise_name(
    admission.time_since_first_vaccine_dose),
  "c19vaxd2_adm" = .normalise_name(
    admission.time_since_second_vaccine_dose),
  "c19vaxd3_adm" = .normalise_name(
    admission.time_since_third_vaccine_dose),
  
  #### Genomics ----
  
  "c19_variant" = .normalise_variant(
    genomic.variant),
  
  #### Admission symptoms signs: ----
  ## TODO: range and data quality checks
  "immsup" = .normalise_yesno(
    admission.on_immunosuppression),
  "psi_class" = .normalise_list(
    admission.pneumonia_severity_index_class, c("Class I","Class II","Class III","Class IV","Class V"),ordered=TRUE),
  "crb_test_mai" = .normalise_list(
    admission.curb_65_severity_score,c("0-Very Low","1-Low","2-Moderate","3-Severe","4-Severe","5-Severe"),zeroValue = TRUE,ordered=TRUE),
  "news_2_total" = .normalise_name(
    admission.news2_score),
  "pulse_ox" = .normalise_name(
    admission.oximetry),
  "rr" = .normalise_name(
    admission.respiratory_rate),
  "fio2" = .normalise_name(
    admission.max_oxygen),
  "systolic_bp" = .normalise_name(
    admission.systolic_bp),
  "diastolic_bp" = .normalise_name(
    admission.diastolic_bp),
  "hr" = .normalise_name(
    admission.heart_rate),
  "temperature" = .normalise_list(
    admission.temperature, c("Normal","Fever (T>38.0°C)","Hypothermia (T< 35.5°C)","Not recorded"), codes = c(3,1,2,4)),
  "symptom_days_preadmit" = .normalise_name(
    admission.duration_symptoms),
  "previous_infection" = .normalise_list(
    admission.previous_covid_infection, c("yes","no","unknown")),
  "previousinfection_date" = .normalise_date(
    admission.previous_covid_infection_date),
  "c19d_preadm" = .normalise_name(
    admission.time_since_covid_diagnosis),
  "admission_date" = .normalise_date(
    admission.date),
  "rockwood" = .normalise_name(
    admission.rockwood_score),
  "cci_total_score" = .normalise_name(
    admission.charlson_comorbidity_index),
  "height" = .normalise_name(
    admission.height),
  "weight" = .normalise_name(
    admission.weight),
  "bmi" = .normalise_double(
    admission.BMI, limits=c(15,45)),
  "first_radio" = .normalise_checkboxes(vars(
    admission.cxr_normal,
    admission.cxr_pneumonia,
    admission.cxr_heart_failure,
    admission.cxr_pleural_effusion,
    admission.cxr_covid_changes,
    admission.cxr_other
  )),
  
  
  
  #### Day 7 follow up ----
  "c19_peep" = .normalise_name(
    day_7.max_peep),
  "c19_hospadm" = .normalise_list(
    day_7.length_of_stay, c("0 day","1 day","2 days","3 days","4 days","5 days","6 days","7 days","still inpatient"),ordered=TRUE,zeroValue = TRUE),
  "c17_high" = .normalise_list(
    day_7.max_care_level, c("Routine ward","High care area","ICU/HDU")),
  "c19icuon" = .normalise_yesno(
    day_7.still_on_icu),
  "c19_icudays" = .normalise_list(
    day_7.icu_length_of_stay, c("0 day","1 day","2 days","3 days","4 days","5 days","6 days","7 days"),ordered=TRUE,zeroValue = TRUE),
  "c19_vent" = .normalise_list(
    day_7.max_ventilation_level, c("None","Oxygen","HFNO","NIV (CPAP/BiPAP)","ETT"),ordered=TRUE),
  "c19_ox" = .normalise_list(
    day_7.max_o2_level, c("room air","24-28%","30-35%","40%","50%","60%","80%","Over 80%"),ordered=TRUE),
  "c19_ionotropes" = .normalise_yesno(
    day_7.ionotropes_needed),
  "c19_complication" = .normalise_checkboxes(vars(
    day_7.PE,
    day_7.DVT,
    day_7.ARF,
    day_7.NSTEMI,
    day_7.STEMI,
    day_7.cardiac_failure,
    day_7.new_AF,
    day_7.new_other_arrythmia,
    day_7.inpatient_fall,
    day_7.other_complication,
    day_7.no_complication
  )),
  "c19_death7d" = .normalise_yesno(
    day_7.death),
  "c19_meds" = .normalise_checkboxes(vars(
    treatment.dexamethasone,
    treatment.remdesevir,
    treatment.tocilizumab,
    treatment.sarilumab,
    treatment.in_drug_trial,
    treatment.no_drug_treatment
  )),
  
  #### Long term follow up ----
  "hospital_length_of_stay" = .normalise_name(
    outcome.length_of_stay),
  "survival_days" = .normalise_name(
    outcome.survival_duration),
  "ip_death" = .normalise_name(
    outcome.inpatient_death_days),
  "days_in_icu" = .normalise_name(
    outcome.icu_duration),
  "did_the_patient_have_respi" = .normalise_name(
    outcome.respiratory_support_needed),
  "number_of_days_of_ventilat" = .normalise_name(
    outcome.ventilator_duration),
  "ett_days" = .normalise_name(
    outcome.endotracheal_tube_duration),
  "renal_replacement_therapy" = .normalise_name(
    outcome.renal_support_duration),
  "complications" = .normalise_checkboxes(vars(
    outcome.acute_renal_failure,
    outcome.liver_dysfunction,
    outcome.hospital_acquired_infection,
    outcome.acute_respiratory_distress_syndrome,
    outcome.NSTEMI,
    outcome.STEMI,
    outcome.new_af,
    outcome.new_other_arrhthmia,
    outcome.stroke,
    outcome.DVT,
    outcome.PE,
    outcome.heart_failure,
    outcome.fall_in_hospital,
    outcome.reduced_mobility,
    outcome.increasing_care_requirement,
    outcome.no_complications
  )),
  "ventilatory_support" = .normalise_list(
    outcome.highest_level_ventilatory_support, c("Intubation","BiPAP","CPAP","High-Flow Nasal Cannulae","None"),ordered=TRUE
  ),
  
  
  #### Comorbidities ----
  "resp_disease" = .normalise_checkboxes(vars(
    comorbid.no_resp_dx,
    comorbid.copd,
    comorbid.asthma,
    comorbid.resp_other,
  )),
  "other_respiratory_disease" = .normalise_checkboxes(vars(
    comorbid.bronchiectasis,
    comorbid.interstitial_lung_dx,
    comorbid.cystic_fibrosis,
    comorbid.pulmonary_hypertension,
    comorbid.chronic_pleural_dx,
    comorbid.other_chronic_resp_dx,
  )),
  "chd" = .normalise_checkboxes(vars(
    comorbid.no_heart_dx,
    comorbid.ccf,
    comorbid.ihd,
    comorbid.hypertension,
    comorbid.other_heart_dx
  )),
  "mi" = .normalise_yesno(
    comorbid.previous_mi
  ),
  "other_chd" = .normalise_checkboxes(vars(
    comorbid.congenital_heart_dx,
    comorbid.af,
    comorbid.other_arrythmia,
    comorbid.pacemaker,
    comorbid.valvular_heart_dx,
    comorbid.other_other_heart_dx
  )),
  "diabetes" = .normalise_list(
    comorbid.diabetes, c(
      "None","Type 1 - no complications","Type 1 - complications",
      "Type 2 - no complications","Type 2 - complications"
    )),
  "dm_meds" = .normalise_list(
    comorbid.diabetes_medications, c(
      "Oral","Insulin"
    )),
  "neurological_disease" = .normalise_checkboxes(vars(
    comorbid.neuro_other,
    comorbid.cva,
    comorbid.tia,
    comorbid.hemiplegia,
    comorbid.paraplegia,
    comorbid.no_neuro_dx
  )),
  "dementia" = .normalise_checkboxes(vars(
    comorbid.no_dementia,
    comorbid.dementia,
    comorbid.cognitive_impairment
  )),
  "cancer" = .normalise_list(
    comorbid.solid_cancer, c(
      "None", "Solid Organ Cancer - no mets", "Solid Organ Cancer - Metastatic Disease"
    )),
  "haem_malig" = .normalise_checkboxes(vars(
    comorbid.no_haemotological_cancer,
    comorbid.leukaemia,
    comorbid.lymphoma
  )),
  "ckd" = .normalise_list(
    comorbid.ckd, c("None", "Mild (CKD 1-3)","Moderate or Severe CKD (CKD 4+)")),
  "liver_disease" = .normalise_list(
    comorbid.liver_disease, c("None","Liver disease without failure","Liver disease with failure")),
  "gastric_ulcers" = .normalise_yesno(
    comorbid.gastric_ulcers),
  "pvd" = .normalise_yesno(
    comorbid.periph_vasc_dx),
  "ctd" = .normalise_yesno(
    comorbid.connective_tissue_dx),
  "immunodeficiency" = .normalise_yesno(
    comorbid.immunodeficiency),
  "other_pn_disease" = .normalise_yesno(
    comorbid.other_pneumococcal_risks),
  "transplant" = .normalise_yesno(
    comorbid.transplant_recipient),
  "pregnancy" = .normalise_list(
    comorbid.pregnancy, c("Not pregnant","First Trimester","Second Trimester",
                          "Third Trimester","unsure of trimester","Post-partum")),
  "hiv" = .normalise_checkboxes(vars(
    comorbid.no_HIV,
    comorbid.HIV,
    comorbid.AIDS
  )) 
)