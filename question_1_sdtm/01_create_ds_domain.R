# =============================================================
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
# Programmer: Xinran Qi
# Date: May 18 2026
# Objective: SDTM DS Domain Creation using {sdtm.oak}
# =============================================================

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)

# ------ Step 1: Input raw data ------
ds_raw <- pharmaverseraw::ds_raw

# Inspect actual column names for reference
# names(ds_raw) returns:
# STUDY, PATNUM, SITENM, INSTANCE, FORM, FORML,
# IT.DSTERM, IT.DSDECOD, OTHERSP, DSDTCOL,
# DSTMCOL, IT.DSSTDAT, DEATHDT

# ------ Step 2: Define study controlled terminology ------
study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = c("C66727","C66727","C66727","C66727","C66727",
                    "C66727","C66727","C66727","C66727","C66727"),
  term_code = c("C41331","C25250","C28554","C48226","C48227",
                "C48250","C142185","C49628","C49632","C49634"),
  term_value = c("ADVERSE EVENT","COMPLETED","DEATH",
                 "LACK OF EFFICACY","LOST TO FOLLOW-UP",
                 "PHYSICIAN DECISION","PROTOCOL VIOLATION",
                 "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                 "WITHDRAWAL BY SUBJECT"),
  collected_value = c("Adverse Event","Complete","Dead",
                      "Lack of Efficacy","Lost To Follow-Up",
                      "Physician Decision","Protocol Violation",
                      "Trial Screen Failure","Study Terminated By Sponsor",
                      "Withdrawal by Subject"),
  term_preferred_term = c("AE","Completed","Died",NA,NA,NA,
                          "Violation",
                          "Failure to Meet Inclusion/Exclusion Criteria",
                          NA,"Dropout"),
  term_synonyms = c("ADVERSE EVENT","COMPLETE","Death",NA,NA,NA,
                    NA,NA,NA,"Discontinued Participation")
)

# ----- Step 3: Create the requested DS domain with following variables -----
ds <- ds_raw %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0(STUDY, "-", PATNUM),
    DSTERM  = IT.DSTERM,
    DSDECOD = case_when(
      IT.DSTERM == "Adverse Event"               ~ "ADVERSE EVENT",
      IT.DSTERM == "Complete"                    ~ "COMPLETED",
      IT.DSTERM == "Dead"                        ~ "DEATH",
      IT.DSTERM == "Lack of Efficacy"            ~ "LACK OF EFFICACY",
      IT.DSTERM == "Lost To Follow-Up"           ~ "LOST TO FOLLOW-UP",
      IT.DSTERM == "Physician Decision"          ~ "PHYSICIAN DECISION",
      IT.DSTERM == "Protocol Violation"          ~ "PROTOCOL VIOLATION",
      IT.DSTERM == "Trial Screen Failure"        ~ "SCREEN FAILURE",
      IT.DSTERM == "Study Terminated By Sponsor" ~ "STUDY TERMINATED BY SPONSOR",
      IT.DSTERM == "Withdrawal by Subject"       ~ "WITHDRAWAL BY SUBJECT",
      TRUE ~ NA_character_
    ),
    DSCAT = case_when(
      grepl("PROTOCOL", IT.DSDECOD, ignore.case = TRUE) ~ "PROTOCOL-RELATED EVENT",
      TRUE ~ "DISPOSITION EVENT"
    ),
    VISITNUM = NA_real_,
    VISIT    = NA_character_,
    DSDTC    = DSDTCOL,
    DSSTDTC  = IT.DSSTDAT,
    DSSTDY   = NA_real_
  ) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
         DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY)

# ----- Step 4: Save output -----
write.csv(ds, "ds_domain.csv", row.names = FALSE)

# ----- Step 5: Print log as evidence -----
sink("run_log_q1.txt")
cat("DS domain created successfully.\n")
cat("Number of records:", nrow(ds), "\n")
cat("Variables:", paste(names(ds), collapse = ", "), "\n")
print(head(ds))
sink()

cat("Done! Records:", nrow(ds), "\n")
print(head(ds))
