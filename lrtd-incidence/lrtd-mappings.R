.lrtd_mappings = c(
  .mappings,
  "final_soc_lrtd_diagnosis" = .normalise_checkboxes(renameToVars = vars(
    diagnosis.SOC_CAP_radiologically_confirmed,
    diagnosis.SOC_CAP_clinically_confirmed,
    diagnosis.SOC_CAP_no_radiology,
    diagnosis.SOC_LRTI,
    diagnosis.SOC_Empyema_or_abscess,
    diagnosis.SOC_exacerbation_COPD,
    diagnosis.SOC_exacerbation_non_COPD,
    diagnosis.SOC_congestive_heart_failure,
    diagnosis.SOC_non_infectious_process,
    diagnosis.SOC_non_LRTI
  )),
  "covid_19_diagnosis" = .normalise_list(
    diagnosis.covid_19_diagnosis,
    c("COVID-19 - laboratory confirmed","COVID-19 - patient reported test", "COVID-19 - clinical diagnosis (but negative test)",
    "COVID-19 - negative test, unlikely COVID-19 disease","No test performed" )
  ),
  "year" = .normalise_double(admission.year, limits = c(2019,2023)),
  # "week_number" = .normalise_double(admission.week, limits = c(1,53)),
  "study_week" = .normalise_double(admission.study_week),
  "c19_adm_swab" = .normalise_checkboxes_to_list(
    diagnosis.admission_swab_old,
    values = c("COVID-19 positive","COVID-19 negative","Indeterminate","Known community/recent positive","Not performed")
  ),
  "ppv23" = .normalise_list(
    vaccination.pneumovax,
    c("Not received","Received","Unknown"), codes=c(2,1,3)
  ),
  "flu_vaccine" = .normalise_list(
    vaccination.influenza_vaccination,
    c("Not received","Received","Unknown"), codes=c(2,1,3)
  ),
  "consented" = .normalise_list(
    admin.consented,
    c("Not approached","Yes","Declined consent"), zeroValue = TRUE
  ),
  "ppc" = .normalise_list(
    admin.pp_consented,
    c("Not approached","Yes","Declined consent"), zeroValue = TRUE
  ),
  "withdrawal" = .normalise_yesno(
    admin.withdrawal
  )
  
)