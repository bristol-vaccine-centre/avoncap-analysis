here::i_am("tests/testthat/data-integrity.R")

source(here::here("common-setup.R"))
out = outputter(directory=here::here("output/tests"))
# make sure we are looking at most up to date data
options(reproduce.at=NULL)

# load common data - this is the UoB RedCap data
lrtd_data = load_data("AvonCAPLRTDCentralDa")
lrtd_norm = lrtd_data %>% normalise_data(remove_mapped = FALSE, remove_unmapped = FALSE, .nocache=TRUE)
lrtd_aug = lrtd_norm %>% augment_data()

# load common data - this is the NHS RedCap partial extract
data_raw = load_data("DeltaVE")
data_norm = data_raw %>% normalise_data(remove_mapped = FALSE, remove_unmapped = FALSE)
data_aug = data_norm %>% augment_data()

canonical_admissions = load_data("AdmissionDateByStudyNumber") %>% 
  mutate(
    canonical.admission_date = as.Date(admission_date,"%d/%m/%Y"),
    canonical.study_week = study_week(canonical.admission_date),
    canonical.week_number = lubridate::epiweek(canonical.admission_date),
    canonical.year = lubridate::year(canonical.admission_date)
  )

test_that("UoB raw data approx complete", {
  missing = canonical_admissions %>% anti_join(lrtd_data, by="record_number")
  # ggplot(missing,aes(x=canonical.admission_date))+geom_histogram(binwidth = 1)
  missing %>% select(record_number) %>% readr::write_csv(out("missing_in_UoB_RedCap.csv"))
  testthat::expect(nrow(missing)/nrow(canonical_admissions) < 0.01, ">1% of cases are missing from the UoB data set")
})

test_that("UoB date inference approx correct", {
  matches = canonical_admissions %>% inner_join(lrtd_aug, by=c("record_number"="..record_number"))
  matches = matches %>% mutate(
    delta = abs(canonical.study_week-admission.study_week),
    status = case_when(
      is.na(delta) ~ "missing",
      delta > 1 ~ "mismatched",
      delta == 1 ~ "partial mismatch",
      TRUE ~ "matched"
    )
  ) 
  mismatch = matches %>% filter(status %in% c("missing","mismatched"))
  ggplot(mismatch,aes(x=canonical.admission_date, fill=as.factor(status)))+geom_histogram(binwidth = 1)+scale_x_date(date_breaks = "2 week")
  mismatch %>% select(
    record_number, canonical.admission_date, canonical.week_number, canonical.year,
    status, actual.week_number = .week_number, ..enrollment_date, 
    estimated.year = ..year,
    estimated.admission_date = admission.date) %>% 
    readr::write_csv(out("week_number_admission_date_mismatches_in_UoB_RedCap.csv"))
  testthat::expect(nrow(mismatch)/nrow(matches) < 0.01, ">1% of cases are missing week_number or week_number is not consistent with admission_date")
})

test_that("UoB ", {
  matches = canonical_admissions %>% inner_join(lrtd_aug, by=c("record_number"="..record_number"))
  admittedAfterEnrolled = matches %>% filter(
    canonical.admission_date > ..enrollment_date,
  ) 
  admittedAfterEnrolled %>% select(record_number, canonical.admission_date, ..enrollment_date) %>% 
    readr::write_csv(out("enrolled_date_admission_date_inconsistent_in_UoB_RedCap.csv"))
  testthat::expect(nrow(admittedAfterEnrolled)/nrow(matches) < 0.01, ">1% of cases are missing week_no or week_no not consistent with admission_date")
})