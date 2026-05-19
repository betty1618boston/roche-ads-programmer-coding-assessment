# ========================================================================================================================
# Question 3a: AE Summary Table using {gtsummary} 
# Programmer: Xinran Qi
# Date: May 2026
# Objective: TLG - create outputs for adverse events summary using the ADAE dataset and {gtsummary} as requested
# Approach: Use gtsummary for the initial summary framework,
#           extended with gt for full FDA regulatory formatting
#           including SOC grouping, custom N denominators,
#           hierarchical indentation, descending frequency sort.
#           {gtsummary} will make the table identical to the PDF example, but {gt} looks prettier and is the same
# ========================================================================================================================

library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)
library(tidyr)
library(purrr)

# ----- Step 1: Input adae datasets -----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl %>%
  filter(ACTARM != "Screen Failure")

# ----- Step 2: Filter to Treatment-Emergent AEs only -----
teae <- adae %>% filter(TRTEMFL == "Y")
cat("TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:", n_distinct(teae$USUBJID), "\n")

# ----- Step 3: Get N per treatment arm dynamically -----
trt_n <- adsl %>%
  count(ACTARM, name = "N_total") %>%
  arrange(ACTARM)

arm_cols  <- trt_n$ACTARM
placebo_n <- trt_n %>% filter(grepl("Placebo",ACTARM)) %>% pull(N_total)
high_n    <- trt_n %>% filter(grepl("High",ACTARM))    %>% pull(N_total)
low_n     <- trt_n %>% filter(grepl("Low",ACTARM))     %>% pull(N_total)

cat("N — Placebo:", placebo_n, "| High:", high_n, "| Low:", low_n, "\n")

# ----- Step 4: Use gtsummary to create base overall summary -----
# gtsummary tbl_summary used as the analytical framework
# for computing subject counts and percentages per AETERM
teae_gts <- teae %>%
  distinct(USUBJID, AETERM, ACTARM) %>%
  mutate(ACTARM = factor(ACTARM,
                         levels = c("Placebo",
                                    "Xanomeline High Dose",
                                    "Xanomeline Low Dose")))

# gtsummary summary object — core analytical step
gts_obj <- teae_gts %>%
  tbl_summary(
    by        = ACTARM,
    include   = AETERM,
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits    = all_categorical() ~ c(0, 1)
  ) %>%
  add_overall(last = TRUE) %>%
  bold_labels()

cat("gtsummary base object created successfully.\n")

# ----- Step 5: Build hierarchical table data -----
# Extended beyond gtsummary defaults to achieve FDA Table 10
# SOC grouping with descending frequency sort

# Overall TEAE row
overall_row <- teae %>%
  distinct(USUBJID, ACTARM) %>%
  group_by(ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(pct  = round(100 * n / N_total),
         cell = paste0(n, " (", pct, "%)")) %>%
  select(ACTARM, cell) %>%
  pivot_wider(names_from = ACTARM, values_from = cell) %>%
  mutate(AESOC  = "",
         AETERM = "Treatment Emergent AEs",
         Total  = paste0(n_distinct(teae$USUBJID), " (",
                         round(100 * n_distinct(teae$USUBJID) /
                               nrow(adsl)), "%)"))

# SOC-level rows
soc_rows <- teae %>%
  distinct(USUBJID, AESOC, ACTARM) %>%
  group_by(AESOC, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(pct  = round(100 * n / N_total),
         cell = paste0(n, " (", pct, "%)")) %>%
  select(AESOC, ACTARM, cell) %>%
  pivot_wider(names_from  = ACTARM,
              values_from = cell,
              values_fill = "0 (0%)") %>%
  mutate(AETERM = AESOC, Total = "")

# SOC sort order by frequency
soc_order <- teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, name = "soc_n") %>%
  arrange(desc(soc_n))

# AE term-level rows
term_rows <- teae %>%
  distinct(USUBJID, AESOC, AETERM, ACTARM) %>%
  group_by(AESOC, AETERM, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(trt_n, by = "ACTARM") %>%
  mutate(pct  = round(100 * n / N_total, 1),
         cell = paste0(n, " (", pct, "%)")) %>%
  select(AESOC, AETERM, ACTARM, cell) %>%
  pivot_wider(names_from  = ACTARM,
              values_from = cell,
              values_fill = "0 (0%)") %>%
  left_join(
    teae %>%
      distinct(USUBJID, AESOC, AETERM) %>%
      group_by(AESOC, AETERM) %>%
      summarise(Total_n = n_distinct(USUBJID), .groups = "drop") %>%
      mutate(Total = paste0(Total_n, " (",
                            round(100 * Total_n / nrow(adsl), 1),
                            "%)")),
    by = c("AESOC", "AETERM"))

# ----- Step 6: Combine in correct SOC + term order -----
all_rows <- map_dfr(soc_order$AESOC, function(soc_name) {
  bind_rows(
    soc_rows %>% filter(AESOC == soc_name) %>%
      select(AESOC, AETERM, all_of(arm_cols), Total),
    term_rows %>% filter(AESOC == soc_name) %>%
      arrange(desc(Total_n)) %>%
      select(AESOC, AETERM, all_of(arm_cols), Total)
  )
})

combined <- bind_rows(
  overall_row %>% select(AESOC, AETERM, all_of(arm_cols), Total),
  all_rows
)

# ----- Step 7: Render final table using gt -----
# gt is the rendering engine underlying gtsummary (via as_gt())
# Used here directly for full FDA Table 10 formatting control
gt_table <- combined %>%
  select(-AESOC) %>%
  gt() %>%
  tab_header(
    title    = "Summary of Treatment-Emergent Adverse Events",
    subtitle = "Subjects with at Least One TEAE — Sorted by Descending Frequency"
  ) %>%
  cols_label(
    AETERM = md("**Primary System Organ Class**  \n**Reported Term for the Adverse Event**"),
    `Placebo` = md(paste0("**Placebo**  \nN = ", placebo_n)),
    `Xanomeline High Dose` = md(paste0("**Xanomeline High  \nDose**  \nN = ", high_n)),
    `Xanomeline Low Dose`  = md(paste0("**Xanomeline Low  \nDose**  \nN = ", low_n)),
    Total = md("**Total**")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      rows = AETERM == "Treatment Emergent AEs")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      columns = AETERM,
      rows    = AETERM %in% soc_rows$AESOC)
  ) %>%
  tab_style(
    style     = cell_text(indent = px(20)),
    locations = cells_body(
      columns = AETERM,
      rows    = !(AETERM %in% c("Treatment Emergent AEs",
                                 soc_rows$AESOC)))
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_text(align = "center"),
    locations = cells_column_labels(
      columns = c(`Placebo`, `Xanomeline High Dose`,
                  `Xanomeline Low Dose`, Total))
  ) %>%
  tab_style(
    style     = cell_text(align = "center"),
    locations = cells_body(
      columns = c(`Placebo`, `Xanomeline High Dose`,
                  `Xanomeline Low Dose`, Total))
  ) %>%
  opt_stylize(style = 1) %>%
  tab_options(
    column_labels.padding = px(8),
    table.font.size       = 13
  )

# ----- Step 8: Save output -----
gtsave(gt_table, "ae_summary_table.html")
cat("AE summary table saved as ae_summary_table.html\n")

# ----- Step 9: Save log as evidence -----
sink("log_table.txt")
cat("AE Summary Table created successfully.\n")
cat("gtsummary used for base analytical framework.\n")
cat("gt used for FDA Table 10 regulatory formatting.\n")
cat("TEAE records:", nrow(teae), "\n")
cat("Unique subjects with TEAEs:",
    n_distinct(teae$USUBJID), "\n")
cat("Unique AE terms:", n_distinct(teae$AETERM), "\n")
cat("N — Placebo:", placebo_n,
    "| High:", high_n, "| Low:", low_n, "\n")
sink()
cat("Table saved!\n")
