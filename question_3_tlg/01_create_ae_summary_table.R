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
teae <- adae %>% filter(TRTEMFL == "Y")
cat("TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:", n_distinct(teae$USUBJID), "\n")

# ----- Step 3: Get N per treatment arm for column headers -----
trt_n <- adsl %>%
  count(ACTARM, name = "N_total") %>%
  arrange(ACTARM)

# Extract N values dynamically from actual data
placebo_n <- trt_n %>%
  filter(grepl("Placebo", ACTARM, ignore.case = TRUE)) %>%
  pull(N_total)
high_n <- trt_n %>%
  filter(grepl("High", ACTARM, ignore.case = TRUE)) %>%
  pull(N_total)
low_n <- trt_n %>%
  filter(grepl("Low", ACTARM, ignore.case = TRUE)) %>%
  pull(N_total)

cat("N per arm — Placebo:", placebo_n,
    "| High Dose:", high_n,
    "| Low Dose:", low_n, "\n")

# ----- Step 4: Build overall TEAE summary row -----
overall_row <- teae %>%
  distinct(USUBJID, ACTARM) %>%
  group_by(ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(
    pct  = round(100 * n / N_total),
    cell = paste0(n, "\n(", pct, "%)")
  ) %>%
  select(ACTARM, cell) %>%
  pivot_wider(names_from = ACTARM, values_from = cell) %>%
  mutate(
    AESOC  = "",
    AETERM = "Treatment Emergent AEs",
    Total  = paste0(
      n_distinct(teae$USUBJID), " (",
      round(100 * n_distinct(teae$USUBJID) /
            n_distinct(adsl$USUBJID)), "%)")
  )


# ----- Step 5: Build SOC-level rows -----
soc_rows <- teae %>%
  distinct(USUBJID, AESOC, ACTARM) %>%
  group_by(AESOC, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(
    pct  = round(100 * n / N_total),
    cell = paste0(n, "\n(", pct, "%)")
  ) %>%
  select(AESOC, ACTARM, cell) %>%
  pivot_wider(names_from  = ACTARM,
              values_from = cell,
              values_fill = "0\n(0%)") %>%
  mutate(AETERM = AESOC, Total = "")

# Get SOC sort order by frequency
soc_order <- teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, name = "soc_n") %>%
  arrange(desc(soc_n))

# ----- Step 6: Build AE term-level rows -----
term_rows <- teae %>%
  distinct(USUBJID, AESOC, AETERM, ACTARM) %>%
  group_by(AESOC, AETERM, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(
    pct  = round(100 * n / N_total, 1),
    cell = paste0(n, " (", pct, "%)")
  ) %>%
  select(AESOC, AETERM, ACTARM, cell) %>%
  pivot_wider(names_from  = ACTARM,
              values_from = cell,
              values_fill = "0 (0%)") %>%
  left_join(
    teae %>%
      distinct(USUBJID, AESOC, AETERM) %>%
      group_by(AESOC, AETERM) %>%
      summarise(Total_n = n_distinct(USUBJID), .groups = "drop") %>%
      mutate(
        Total = paste0(Total_n, " (",
                       round(100 * Total_n /
                             n_distinct(adsl$USUBJID), 1), "%)")
      ),
    by = c("AESOC", "AETERM")
  )
# ----- Step 7: Combine rows in correct order -----
# Order: Overall row, then SOC + its terms sorted by frequency
arm_cols <- trt_n$ACTARM

combined <- bind_rows(
  overall_row %>% select(AESOC, AETERM,
                          all_of(arm_cols), Total),
  soc_order %>%
    left_join(
      bind_rows(
        soc_rows %>%
          select(AESOC, AETERM, all_of(arm_cols), Total),
        term_rows %>%
          select(AESOC, AETERM, all_of(arm_cols),
                 Total, Total_n)
      ),
      by = "AESOC"
    ) %>%
    arrange(AESOC, desc(Total_n)) %>%
    select(AESOC, AETERM, all_of(arm_cols), Total)
)

# ----- Step 8: Render as gt table -----
gt_table <- combined %>%
  select(-AESOC) %>%
  gt() %>%
  tab_header(
    title    = "Summary of Treatment-Emergent Adverse Events",
    subtitle = "Subjects with at Least One TEAE — Sorted by Descending Frequency"
  ) %>%
  cols_label(
    AETERM               = md("**Primary System Organ Class**\nReported Term for the Adverse Event"),
    `Placebo`            = md(paste0("**Placebo**\nN = ", placebo_n)),
    `Xanomeline High Dose` = md(paste0("**Xanomeline High\nDose**\nN = ", high_n)),
    `Xanomeline Low Dose`  = md(paste0("**Xanomeline Low\nDose**\nN = ", low_n)),
    Total                = md("**Total**")
  ) %>%
  # Bold the overall TEAE row
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      columns = everything(),
      rows    = AETERM == "Treatment Emergent AEs"
    )
  ) %>%
  # Bold SOC rows (rows where AETERM matches AESOC values)
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      columns = AETERM,
      rows    = AETERM %in% soc_rows$AESOC
    )
  ) %>%
  # Indent term-level rows
  tab_style(
    style     = cell_text(indent = px(20)),
    locations = cells_body(
      columns = AETERM,
      rows    = !(AETERM %in% c("Treatment Emergent AEs",
                                 soc_rows$AESOC))
    )
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  opt_stylize(style = 1) %>%
  tab_options(
    table.font.size       = 13,
    column_labels.padding = px(8)
  )

# ----- Step 9: Save output -----
gtsave(gt_table, "ae_summary_table.html")
cat("AE summary table saved.\n")

# ----- Step 10: Save log -----
sink("log_table.txt")
cat("AE Summary Table created successfully.\n")
cat("TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:",
    n_distinct(teae$USUBJID), "\n")
cat("Unique AE terms:", n_distinct(teae$AETERM), "\n")
cat("N per arm — Placebo:", placebo_n,
    "| High:", high_n, "| Low:", low_n, "\n")
sink()
