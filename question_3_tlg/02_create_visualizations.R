# =======================================================================================================
# Question 3b: AE reporting - Data visualizations using {ggplot2}
# Programmer: Xinran Qi
# Date: May 18 2026
# Objective: TLG - create outputs for adverse events summary using the ADAE dataset and {ggplot2}
# =======================================================================================================

library(ggplot2)
library(pharmaverseadam)
library(dplyr)
library(scales)

# ------ Step 1: Input ADAE datasets ------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Total number of subjects (denominator for percentages)
n_subjects <- n_distinct(adsl$USUBJID)
cat("Total subjects:", n_subjects, "\n")

# ------ Step 2: Plot 1 — AE Severity Distribution by Treatment ------
# Shows count of AEs broken down by severity within each treatment arm
# AESEV variable contains: MILD, MODERATE, SEVERE

severity_data <- adae %>%
  filter(TRTEMFL == "Y", !is.na(AESEV)) %>%
  group_by(ACTARM, AESEV) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(AESEV = factor(AESEV,
                        levels = c("MILD", "MODERATE", "SEVERE")))

plot1 <- ggplot(severity_data,
                aes(x = ACTARM, y = count, fill = AESEV)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("MILD"     = "#F4A582",
               "MODERATE" = "#4DAC26",
               "SEVERE"   = "#74ADD1"),
    name = "Severity/Intensity"
  ) +
  labs(
    title = "AE Severity Distribution by Treatment",
    x     = "Treatment Arm",
    y     = "Count of AEs"
  ) +
  theme_bw() +
  theme(
    axis.text.x     = element_text(angle = 15, hjust = 1),
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "right"
  )

# Save Plot 1
ggsave("plot1_ae_severity.png",
       plot  = plot1,
       width = 8, height = 6, dpi = 300)
cat("Plot 1 saved: plot1_ae_severity.png\n")

# ------- Step 3: Plot 2 — Top 10 Most Frequent Adverse Events ------
# Uses Clopper-Pearson exact method for confidence intervals
# This is the standard method for binomial proportions in clinical trials

top10_ae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  distinct(USUBJID, AETERM) %>%
  group_by(AETERM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    prop    = n / n_subjects,
    # Clopper-Pearson 95% confidence interval
    ci_low  = qbeta(0.025, n, n_subjects - n + 1),
    ci_high = qbeta(0.975, n + 1, n_subjects - n)
  ) %>%
  arrange(desc(n)) %>%
  slice(1:10)

plot2 <- ggplot(top10_ae,
                aes(x = prop,
                    y = reorder(AETERM, prop))) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    height = 0.3, color = "black"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects,
                      " subjects; 95% Clopper-Pearson CIs"),
    x        = "Percentage of Patients (%)",
    y        = NULL
  ) +
  theme_bw() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y   = element_text(size = 9)
  )

# Save Plot 2
ggsave("plot2_top10_ae.png",
       plot  = plot2,
       width = 8, height = 6, dpi = 300)
cat("Plot 2 saved: plot2_top10_ae.png\n")

# ----- Step 4: Save log -----
sink("log_viz.txt")
cat("Visualizations created successfully.\n")
cat("Total subjects:", n_subjects, "\n")
cat("Plot 1: AE severity distribution by treatment arm\n")
cat("Plot 2: Top 10 most frequent AEs with 95% Clopper-Pearson CIs\n")
cat("Top 10 AEs:\n")
print(top10_ae %>% select(AETERM, n, prop))
sink()
