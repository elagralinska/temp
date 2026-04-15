############
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
############

## Part 1: installing packages and setup

log_con <- file("Q1_logfile.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)
library(tidyverse)

cat("Step 1 complete: loading libraries\n")

## Part 2: Load all data and create oak_id_vars
ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

study_ct <- read_csv("sdtm_ct.csv", show_col_types = FALSE)

cat("Step 2 complete: loading data\n")

## Part 3: Map Topic Variable

# DS topic variable = DSTERM

ds <-
  assign_no_ct(
    # Derive topic variable
    # Map DSTERM using assign_no_ct, raw_var=IT.DSTERM, tgt_var=DSTERM
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",   
    tgt_var = "DSTERM",
#   ct_spec = study_ct,
#   ct_clst = "C66727",
    id_vars = oak_id_vars()
  )

cat("Step 3 complete: mapping topic variable\n")

## Part 4: Map Rest of Variables


ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSTERM = dplyr::if_else(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "",
      as.character(ds_raw$OTHERSP),
      as.character(DSTERM)
    ),
    DSDECOD = dplyr::if_else(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "",
      as.character(ds_raw$OTHERSP),
      as.character(DSDECOD)
    )
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSCAT = dplyr::if_else(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "",
      "OTHER EVENT",
      dplyr::if_else(
        ds_raw$IT.DSDECOD == "Randomized",
        "PROTOCOL MILESTONE",
        "DISPOSITION EVENT"
      )
    )
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, INSTANCE == "Ambul Ecg Removal"),
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    tgt_val = "AMBUL ECG REMOVAL",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    VISITNUM = if_else(
      grepl("^UNSCHEDULED", VISITNUM),
      gsub("^UNSCHEDULED\\s*", "", VISITNUM),
      VISITNUM
    )
  ) %>%
  mutate(
    VISITNUM = if_else(
      grepl("AMBUL ECG REMOVAL", VISIT, ignore.case = TRUE),
      "6",
      as.character(VISITNUM)
    )
  )
  
cat("Step 4 complete: mapping rest of variables\n")

## Part 5: Create SDTM derived variables

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY, 
    DOMAIN = "DS", 
    USUBJID = paste0("01-", ds_raw$PATNUM), 
    VISIT = toupper(VISIT),
    DSDECOD = toupper(DSDECOD)
  ) %>%
    derive_seq( 
               tgt_var = "DSSEQ",
               rec_vars = c("USUBJID", "DSTERM")
    ) %>%
    derive_study_day( 
      sdtm_in = .,
      dm_domain = dm,
      tgdt = "DSSTDTC",
      refdt = "RFXSTDTC",
      study_day_var = "DSSTDY"
    ) %>%
    select(
     "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
     "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
    )
    
cat("Step 5 complete: creating SDTM derived variables\n")

## Part 6: Export

write.csv(ds, "ds.csv", row.names = FALSE)

cat("Step 6 complete: exporting ds.csv\n")

sink(type = "message")
sink()
close(log_con)
