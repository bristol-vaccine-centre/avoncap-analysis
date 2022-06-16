## Shared dataset definitions ----

avoncap_variants = function(avoncap_original = NULL) { 
  
  if (is.null(avoncap_original)) {
    avoncap_raw = load_data() 
    avoncap_original = avoncap_raw %>% normalise_data()
  }
  
  
  #filedate = format(as.Date(attr(avoncap_raw,"date")),"%d/%m/%Y")
  reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))
  
  # maxDelta = avoncap_original %>% filter(genomic.variant == "Delta") %>% summarise(max = max(admission.date)) %>% pull(max)
  # minOmicron = avoncap_original %>% filter(genomic.variant == "Omicron") %>% summarise(min = min(admission.date)) %>% pull(min)
  maxDelta = as.Date("2022-02-07") # tmp2 %>% filter(genomic.variant == "Delta") %>% summarise(max = max(admission.date)) %>% pull(max)
  minOmicron = as.Date("2021-11-07") # tmp2 %>% filter(genomic.variant == "Omicron") %>% summarise(min = min(admission.date)) %>% pull(min)
  
  out = avoncap_original %>% 
    augment_data() %>% 
    avoncap_calculate_qcovid() 
  v = out %>% get_value_sets()
  
  out = out %>% 
    filter(cohort == "case") %>%
    p_clear() %>%
    comment("{.count} COVID admissions \nby {format(reproduce_at-7, '%d/%m/%Y')}") %>%
    capture_exclusions() %>%
    comment("Last Delta case {format(maxDelta, '%d/%m/%Y')}\nFirst Omicron case {format(minOmicron, '%d/%m/%Y')}") %>%
    include_any(
      genomic.variant %in% c("Delta","Omicron") ~ "{.included} with known Delta or Omicron",
      is.na(genomic.variant) & genomic.variant_inferred %in% c("Delta","Omicron") ~ "{.included} with inferred Delta or Omicron"
    ) %>%
    mutate(
      genomic.variant = forcats::fct_drop(genomic.variant)
    ) %>%
    standard_exclusions() %>% 
    comment() %>% 
    group_by(genomic.variant_inferred, .messages = "stratify by variant") %>%
    comment() %>% 
    exclude_all(
      !(vaccination.vaccination %in% c("Unvaccinated","2 doses","3 doses")) ~ "{.excluded} partially vaccinated",
      diagnosis.infection_context == v$diagnosis.infection_context$`Possible nosocomial` ~ "{.excluded} COVID acquired in hospital",
      # comorbid.pregnancy != v$comorbid.pregnancy$`Not pregnant` ~ "{.excluded} cases in pregnant women",
      .stage = "variants exclusions"
    ) %>%
    mutate(
      vaccination.vaccination = forcats::fct_drop(vaccination.vaccination)
    ) %>%
    comment() %>%
    exclude_all(
      is.na(day_7.max_o2_level)~"{.excluded} missing day 7 O2 level outcome",
      is.na(day_7.los_gt_3)~"{.excluded} missing length of stay outcome",
      is.na(day_7.WHO_score_gt_5)~"{.excluded} missing WHO score outcome",
      .stage = "variant severity analysis"
    ) %>% 
    comment()
  
  return(out)
}
