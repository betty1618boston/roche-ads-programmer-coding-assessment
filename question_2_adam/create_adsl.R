# ============================================================================
# Question 2: ADaM ADSL Dataset Creation using {admiral}
# Programmer: Xinran Qi
# Date: May 18 2026
# Objective: Create ADaM ADSL dataset using SDTM with custom derived variables
# ============================================================================

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)

# ------ Step 1: Input SDTM datasets ------
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Convert blanks to NA
dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

# ------ Step 2: Start ADSL from DM domain ------
adsl <- dm %>%
  filter(is.na(ARMCD) | ARMCD != "Scrnfail")

# ------ Step 3: Derive AGEGR9 and AGEGR9N ------
# Age is grouped into the following categories
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18              ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50              ~ ">50",
      TRUE                  ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18              ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50              ~ 3,
      TRUE                  ~ NA_real_
    )
  )

# ------ Step 4: Derive ITTFL (stands for Intent-to-Treat Flag) ------
# Y if patient was randomized (ARM is populated in DM)
adsl <- adsl %>%
  mutate(ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N"))

# ------ Step 5: Derive TRTSDTM and TRTSTMF ------
# Treatment start datetime from first valid exposure record
# Valid dose: EXDOSE > 0 OR (EXDOSE == 0 AND EXTRT contains PLACEBO)
ex_valid <- ex %>%
  filter(
    !is.na(EXSTDTC),
    EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT, ignore.case = TRUE))
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "EXST",
    dtc = EXSTDTC,
    highest_imputation = "h",
    time_imputation = "first"
  ) %>%
  group_by(USUBJID) %>%
  arrange(EXSTDTM) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF)

adsl <- adsl %>%
  left_join(ex_valid, by = "USUBJID")

# ------ Step 6: Derive LSTAVLDT (stands for Last Known Alive Date) ------
# Max of: last vital signs date, last AE date,
#         last disposition date, last treatment date

# (1) Last vital signs date with a valid result
vs_date <- vs %>%
  filter(!is.na(VSDTC),
         !(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  mutate(VS_DT = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(VS_LAST = max(VS_DT, na.rm = TRUE), .groups = "drop")

# (2) Last AE onset date
ae_date <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  mutate(AE_DT = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(AE_LAST = max(AE_DT, na.rm = TRUE), .groups = "drop")

# (3) Last disposition date
ds_date <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  mutate(DS_DT = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(DS_LAST = max(DS_DT, na.rm = TRUE), .groups = "drop")

# (4) Last treatment date from TRTSDTM
trt_date <- adsl %>%
  select(USUBJID, TRTSDTM) %>%
  mutate(TRT_DT = as.Date(TRTSDTM))

# Combine all — take the maximum date across all sources
adsl <- adsl %>%
  left_join(vs_date,  by = "USUBJID") %>%
  left_join(ae_date,  by = "USUBJID") %>%
  left_join(ds_date,  by = "USUBJID") %>%
  left_join(trt_date %>% select(USUBJID, TRT_DT), by = "USUBJID") %>%
  rowwise() %>%
  mutate(
    LSTAVLDT = max(c(VS_LAST, AE_LAST, DS_LAST, TRT_DT),
                   na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-VS_LAST, -AE_LAST, -DS_LAST, -TRT_DT)

# ------ Step 7: Save outputs ------
saveRDS(adsl, "adsl.rds")
write.csv(adsl, "adsl.csv", row.names = FALSE)

# ------ Step 8: Print log ------
sink("run_log.txt")
cat("ADSL created successfully.\n")
cat("Total subjects:", nrow(adsl), "\n")
cat("ITTFL distribution:\n")
print(table(adsl$ITTFL))
cat("\nAGEGR9 distribution:\n")
print(table(adsl$AGEGR9))
cat("\nKey derived variables (first 10 rows):\n")
print(adsl %>%
        select(USUBJID, AGEGR9, AGEGR9N, ITTFL,
               TRTSDTM, TRTSTMF, LSTAVLDT) %>%
        head(10))
sink()

cat("Done. Subjects:", nrow(adsl), "\n")
print(adsl %>%
        select(USUBJID, AGEGR9, AGEGR9N, ITTFL,
               TRTSDTM, LSTAVLDT) %>%
        head(10))
