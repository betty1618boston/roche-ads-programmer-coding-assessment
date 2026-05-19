# =================================================================================================
# Question 3a: AE Reporting using {gtsummary}
# Programmer: Xinran Qi
# Date: May 2026
# Objective: TLG - create outputs for adverse events summary using the ADAE dataset and {gtsummary}
# =================================================================================================

library(gtsummary)
library(pharmaverseadam)
library(dplyr)
library(gt)
library(tidyr)

# ------ Step 1: Input ADaM datasets ------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# ------ Step 2: Filter to Treatment-Emergent AEs only ------
# TRTEMFL == "Y" identifies treatment-emergent adverse events
teae <- adae %>%
  filter(TRTEMFL == "Y")

cat("Total TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:", n_distinct(teae$USUBJID), "\n")

# ------ Step 3: Get counts ------
trt_n <- adsl %>%
  count(ACTARM, name = "N_total")

# ------ Step 4: Get percentage with each AE per treatment arm ------
ae_summary <- teae %>%
  distinct(USUBJID, AESOC, AETERM, ACTARM) %>%
  group_by(AESOC, AETERM, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(
    pct  = round(100 * n / N_total, 1),
    cell = paste0(n, " (", pct, "%)")
  ) %>%
  select(AESOC, AETERM, ACTARM, cell) %>%
  pivot_wider(
    names_from  = ACTARM,
    values_from = cell,
    values_fill = "0 (0.0%)"
  )

# ------ Step 5: Add Total column, AE across all treatment arms combined ------
total_col <- teae %>%
  distinct(USUBJID, AETERM) %>%
  group_by(AETERM) %>%
  summarise(Total_n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    Total = paste0(Total_n, " (",
                   round(100 * Total_n / n_distinct(adsl$USUBJID), 1),
                   "%)")
  )

ae_table <- ae_summary %>%
  left_join(total_col, by = "AETERM") %>%
  arrange(desc(Total_n)) %>%
  select(-Total_n)

# ------ Step 6: Render results as {gt} table with better formats ------
gt_table <- ae_table %>%
  gt() %>%
  tab_header(
    title    = "Summary of Treatment-Emergent Adverse Events",
    subtitle = "Subjects with at Least One TEAE — Sorted by Descending Frequency"
  ) %>%
  cols_label(
    AESOC  = "Primary System Organ Class",
    AETERM = "Reported Term for the Adverse Event",
    Total  = "Total"
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_title()
  )

# ----- Step 7: Save output -----
gtsave(gt_table, "ae_summary_table.html")
cat("AE summary table saved as ae_summary_table.html\n")

# ----- Step 8: Save log -----
sink("log_table.txt")
cat("AE Summary Table created successfully.\n")
cat("Total TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:", n_distinct(teae$USUBJID), "\n")
cat("Unique AE terms:", n_distinct(teae$AETERM), "\n")
cat("Treatment arms included:", paste(unique(teae$ACTARM), collapse = ", "), "\n")
sink()
