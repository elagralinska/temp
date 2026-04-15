## Creating plots

log_con <- file("02_create_visualizations_logfile.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

## 1. Load libraries
library(pharmaverseadam)
library(dplyr)
library(ggplot2)
library(scales)

cat("Step 1 complete: loading libraries\n")

## 2. Filter TEAEs

adae_teae <- pharmaverseadam::adae %>%
  filter(TRTEMFL == "Y")

## 3. Plot 1: AE Severity Distribution by Treatment

plot_dat <- adae_teae %>%
  filter(!is.na(AESEV)) %>%
  count(ACTARM, AESEV, name = "n") %>%
  mutate(
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  )

plot1 <- ggplot(plot_dat, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col() +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Step 2 complete: generating Plot 1\n")

## 4. Plot 2: Top 10 AEs (Overall) with 95% CI

ae_subj <- adae_teae %>%
  distinct(USUBJID, AETERM)

N_total <- ae_subj %>%
  distinct(USUBJID) %>%
  nrow()

ae_top10 <- ae_subj %>%
  count(AETERM, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ae_plot <- ae_top10 %>%
  mutate(
    prop = n / N_total,
    se = sqrt(prop * (1 - prop) / N_total),
    lcl = pmax(0, prop - 1.96 * se),
    ucl = pmin(1, prop + 1.96 * se),
    AETERM = factor(AETERM, levels = rev(AETERM))
  )

plot2 <- ggplot(ae_plot, aes(y = AETERM, x = prop)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = lcl, xmax = ucl),
    orientation = "y",
    height = 0.2
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Top 10 Adverse Events (Overall Incidence with 95% CI)",
    x = "Percentage of Patients (%)",
    y = NULL
  ) 

cat("Step 3 complete: generating Plot 2\n")

## Save plots as PNG

ggsave("plot1_severity.png", plot = plot1, width = 8, height = 5, dpi = 300)
ggsave("plot2_top10AEs.png", plot = plot2, width = 8, height = 5, dpi = 300)

cat("Step 4 complete: saving plots\n")

sink(type = "message")
sink()
close(log_con)
