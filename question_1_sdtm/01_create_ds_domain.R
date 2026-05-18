# =============================================================
# Question 1: SDTM DS Domain Creation using {sdtm.oak}
# Programmer: Xinran Qi
# Date: May 2026
# Purpose: Create DS domain from raw pharmaverseraw::ds_raw
# =============================================================

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)

# ------ Step 1: Input raw data ------
ds_raw <- pharmaverseraw::ds_raw

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
    STUDYID = STUDYID,
    DOMAIN  = "DS",
    USUBJID = USUBJID,
    DSTERM  = DSSPID,
    DSDECOD = case_when(
      DSSPID == "Adverse Event"              ~ "ADVERSE EVENT",
      DSSPID == "Complete"                   ~ "COMPLETED",
      DSSPID == "Dead"                       ~ "DEATH",
      DSSPID == "Lack of Efficacy"           ~ "LACK OF EFFICACY",
      DSSPID == "Lost To Follow-Up"          ~ "LOST TO FOLLOW-UP",
      DSSPID == "Physician Decision"         ~ "PHYSICIAN DECISION",
      DSSPID == "Protocol Violation"         ~ "PROTOCOL VIOLATION",
      DSSPID == "Trial Screen Failure"       ~ "SCREEN FAILURE",
      DSSPID == "Study Terminated By Sponsor"~ "STUDY TERMINATED BY SPONSOR",
      DSSPID == "Withdrawal by Subject"      ~ "WITHDRAWAL BY SUBJECT",
      TRUE ~ NA_character_
    ),
    DSCAT = case_when(
      grepl("PROTOCOL", DSDECOD, ignore.case = TRUE) ~ "PROTOCOL-RELATED EVENT",
      TRUE ~ "DISPOSITION EVENT"
    ),
    VISITNUM = VISITNUM,
    VISIT    = VISIT,
    DSDTC    = DSDTC,
    DSSTDTC  = DSSTDTC,
    DSSTDY   = as.numeric(as.Date(substr(DSSTDTC, 1, 10)) -
                          as.Date(substr(RFSTDTC, 1, 10)))
  ) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
         DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY)

# ----- Step 4: Save output -----
saveRDS(ds, "ds_domain.rds")
write.csv(ds, "ds_domain.csv", row.names = FALSE)

# ----- Step 5: Print log -----
sink("run_log.txt")
cat("DS domain created successfully.\n")
cat("Number of records:", nrow(ds), "\n")
cat("Variables:", paste(names(ds), collapse = ", "), "\n")
print(head(ds))
sink()

cat("Done! Records:", nrow(ds), "\n")
print(head(ds))
