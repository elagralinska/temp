#Question 2
################
log_con <- file("Q2_logfile.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

# Packages
library(admiral)
library(admiraldev)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)


# Input SDTM datasets
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# 1. Base ADSL from DM
adsl <- dm %>%
  select(-DOMAIN)

cat("Step 1 complete: Base ADSL\n")

# 2. ITTFL = Y if DM.ARM not equal to missing
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(ARM %in% c("Screen Failure", "Not Assigned", "Not Treated"), "N", "Y")
  )

cat("Step 2 complete: ITTFL\n")

# 3. Age groups (AGEGR9 / AGEGR9N)
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  )

cat("Step 3 complete: Age groups\n")

# 4. Treatment start datetime (TRTSDTM / TRTSTMF)

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "first",     # ensures 00:00:00
    flag_imputation = "auto"       # handles partial vs full imputation
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "first"
  ) %>% ## if only seconds are missing, do not populate the imputation flag
  mutate(
    EXSTTMF = case_when(
      str_detect(EXSTDTC, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$") ~ NA_character_,
      TRUE ~ EXSTTMF
    )
  )

adsl <- adsl %>%
  # Treatment Start Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Treatment End Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Treatment Start and End Date
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>% # Convert Datetime variables to date
  # Treatment Start Time
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) %>%
  # Treatment Duration
  derive_var_trtdurd()

cat("Step 4 complete: Treatment start datetime\n")

# 5. Last Known Alive Date (LSTAVLDT)

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        ),
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        ),
      ),
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = (!is.na(VSDTC) & (!is.na(VSSTRESN) | !is.na(VSSTRESC))),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        ),
      ),
    event(
      dataset_name = "adsl",
      condition = !is.na(TRTEDTM),
      set_values_to = exprs(
        LSTALVDT = TRTEDTM, seq = 0
      ),
    )
  ),
  source_datasets = list(ae = ae, ds = ds, vs = vs, adsl = adsl),
  tmp_event_nr_var = event_nr,
  order = exprs(LSTALVDT, seq, event_nr),
  mode = "last",
  new_vars = exprs(LSTALVDT)
)

cat("Step 5 complete: Last known alive date\n")


## 6. Export

write.csv(adsl, "adsl.csv", row.names = FALSE)

cat("Step 6 complete: adsl.csv file exported\n")

sink(type = "message")
sink()
close(log_con)
