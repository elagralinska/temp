## Creating ae summary table

log_con <- file("ae_summary_table_logfile.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

library(pharmaverseadam) # for clinical trial data
library(dplyr) # for data manipulation
library(cards) # for creating analysis result displays
library(gtsummary)
#theme_gtsummary_compact() # reduce default padding and font size for a gt table


## 1. load libraries

library(pharmaverseadam)
library(dplyr)
library(gtsummary)

cat("Step 1 complete: loading libraries\n")

# 2. Filtering treatment-emergent adverse events

adae_teae <- pharmaverseadam::adae %>%
  filter(TRTEMFL == "Y")

adsl_saf <- pharmaverseadam::adsl %>%
  filter(SAFFL == "Y")

cat("Step 2 complete: filtering treatment-emergent adverse events\n")

# 3. Keeping one record per subject per AE term per arm

ae <- adae_teae %>%
  distinct(USUBJID, ACTARM, AETERM)

# 4. Create TOTAL column

ae_total <- ae %>%
  mutate(ACTARM = "Total")

ae_all <- bind_rows(ae, ae_total)

ae_all <- ae_all %>%
  mutate(ACTARM = factor(ACTARM, levels = c(
    setdiff(unique(ACTARM), "Total"),
    "Total"
  )))

cat("Step 3 and 4 complete: TOTAL column\n")

# 5. Ordering AETERM by descending frequency (Total column)

ae_order <- ae_all %>%
  filter(ACTARM == "Total") %>%
  count(AETERM, sort = TRUE) %>%
  pull(AETERM)

ae_all <- ae_all %>%
  mutate(AETERM = factor(AETERM, levels = ae_order))

cat("Step 5 complete: ordering\n")

# 6. Build FDA Table

ae_tbl <- ae_all %>%
  tbl_summary(
    by = ACTARM,
    include = AETERM,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  modify_header(label = "**Reported Term for the Adverse Event**") %>%
  bold_labels()

cat("Step 6 complete: building FDA table\n")

# 7. Print table
ae_tbl

library(gt)
ae_gt <- as_gt(ae_tbl)
gtsave(ae_tbl, "ae_summary_table.pdf")

cat("Saving complete\n")

sink(type = "message")
sink()
close(log_con)
