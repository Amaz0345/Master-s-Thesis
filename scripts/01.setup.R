# ============================================================
#  Master Thesis: Setup Script
#  Author: Ananya Mazumder 
#  University of Göttingen 2026 
#  Run this before descriptives, balance tables, regressions
# ============================================================

rm(list = ls())

# ── Libraries ────────────────────────────────────────────────
packages <- c("haven", "tidyverse", "survey", "gtsummary", "flextable", "officer", 
              "broom", "here", "labelled", "tableone")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed]) ##just in case any of these packages are not already installed 

library(haven)
library(tidyverse)
library(survey)
library(gtsummary)
library(flextable)
library(officer)
library(broom)
library(here)
library(labelled)
library(tableone)

# ── Paths ────────────────────────────────────────────────────
setwd("C:/Users/ananyama/Documents/Ananya Thesis")
output_dir <- "C:/Users/ananyama/Documents/Ananya Thesis/outputs"

# ── Load data ────────────────────────────────────────────────
data_2019 <- read_stata("data/data_2019.dta")

# ============================================================
#  DATA CLEANING
# ============================================================

# ── Ethnicity (v131) ─────────────────────────────────────────
data_2019 <- data_2019 %>%
  mutate(
    v131 = to_factor(v131),
    v131 = case_when(
      v131 %in% c("caste", "tribe", "no caste / tribe", "don't know") ~ v131,
      TRUE ~ factor(NA, levels = levels(v131))
    ) %>%
      set_variable_labels(v131 = "ethnicity")
  )

# ── Anthropometrics (v437 = weight, v438 = height) ───────────
# Raw values >9000 are coded missing; divide by 10 to get kg/cm
data_2019 <- data_2019 %>%
  mutate(
    v437 = case_when(v437 < 9000 ~ v437 / 10, TRUE ~ NA_real_),
    v438 = case_when(v438 < 9000 ~ v438 / 10, TRUE ~ NA_real_)
  ) %>%
  set_variable_labels(v437 = "Weight (kg)", v438 = "Height (cm)")

# ── Strip value labels ≥95 (DHS missing codes) ───────────────
# Save labels first so they can be reattached after
# ============================================================
#  VARIABLE CONSTRUCTION
# ============================================================

# ── Age squared ──────────────────────────────────────────────
data_2019 <- data_2019 %>%
  mutate(v012_sq = v012^2)
attr(data_2019$v012_sq, "label") <- "Square of Women's Age"

# ── Eligibility status ───────────────────────────────────────
# 1 = eligible:   age ≥21, first or second child born 2012–2016
# 0 = ineligible: age ≥21, first or second child born 2005–2008
data_2019 <- data_2019 %>%
  mutate(eligibility_status = case_when(
    v012 >= 21 &
      ((b2_01 >= 2012 & b2_01 <= 2016) |
         (b2_02 >= 2012 & b2_02 <= 2016 & !is.na(b2_02))) ~ 1,
    v012 >= 21 &
      ((b2_01 >= 2005 & b2_01 < 2009) |
         (b2_02 >= 2005 & b2_02 < 2009 & !is.na(b2_02))) ~ 0
  ))

# ── Decision-making: binary any-say indicators ───────────────
# 1 = respondent alone or jointly (codes 1 or 2); 0 = otherwise
data_2019 <- data_2019 %>%
  mutate(
    v739_any_say  = ifelse(!is.na(v739),  ifelse(v739  %in% c(1, 2), 1, 0), NA),
    v743a_any_say = ifelse(!is.na(v743a), ifelse(v743a %in% c(1, 2), 1, 0), NA),
    v743b_any_say = ifelse(!is.na(v743b), ifelse(v743b %in% c(1, 2), 1, 0), NA),
    v743d_any_say = ifelse(!is.na(v743d), ifelse(v743d %in% c(1, 2), 1, 0), NA),
    v743f_any_say = ifelse(!is.na(v743f), ifelse(v743f %in% c(1, 2), 1, 0), NA)
  )

# ── Autonomy index and dummy ─────────────────────────────────
# Constructed only for women who answered all 5 decision-making questions
data_2019 <- data_2019 %>%
  mutate(
    n_answered = rowSums(!is.na(across(c(v739_any_say, v743a_any_say,
                                         v743b_any_say, v743d_any_say,
                                         v743f_any_say)))),
    # Index: mean of binary responses (proportion of domains with any say)
    autonomy_index = ifelse(
      n_answered == 5,
      rowMeans(across(c(v739_any_say, v743a_any_say, v743b_any_say,
                        v743d_any_say, v743f_any_say)), na.rm = TRUE),
      NA
    ),
    # Dummy: 1 if woman has say in at least one domain
    autonomy_dummy = ifelse(
      n_answered == 5,
      ifelse(rowSums(across(c(v739_any_say, v743a_any_say, v743b_any_say,
                              v743d_any_say, v743f_any_say)), na.rm = TRUE) > 0, 1, 0),
      NA
    )
  )

# ── DiD group interaction variable ───────────────────────────
data_2019 <- data_2019 %>%
  mutate(
    combined_group = interaction(
      haven::as_factor(eligibility_status),
      haven::as_factor(treated_district),
      sep = " × "
    )
  )

# ── Sampling weights ─────────────────────────────────────────
data_2019 <- data_2019 %>%
  mutate(
    wt_women = v005  / 1000000,
    wt_men   = mv005 / 1000000
  )

# ============================================================
#  VARIABLE LISTS
# ============================================================

controls_hh    <- c("v131","v136","v137","v151","v152",
                    "v119","v120","v121","v122","v123","v124","v125",
                    "mv190","sm190s")

controls_women <- c("v012","v012_sq","v133","v437","v438",
                    "v511","d113","v201","v715","v714")

controls_men   <- c("mv035","mv133","mv212","mv190a","sm190s","mv714")

# Pre-built formula strings 
formula_controls_women <- paste(c(controls_women, controls_hh), collapse = " + ")
formula_controls_men   <- paste(c(controls_men,   controls_hh), collapse = " + ")

# Outcome variable lists
outcomes_dec  <- c("v739","v743a","v743b","v743d","v743f","autonomy_index","autonomy_dummy")
outcomes_ipv  <- c("d106","d107","d108","d129")
outcomes_work <- c("v714", "v714a", "v746", "v731")
outcomes_men_1  <- c("mv633a", "mv633b", "mv633d", "mv634a", "mv634b", "mv634c", "mv634d")
outcomes_men_2 <- c("mv739", "mv743a", "mv743b")
outcomes_men_3  <- c("mv744a", "mv744b", "mv744c", "mv744d", "mv744e")


outcome_labels <- c(
  "v739"  = "Own Earnings",
  "v743a" = "Own Healthcare",
  "v743b" = "Large Purchases",
  "v743d" = "Visiting Family", 
  "v743f" = "Husband's Money",
  "autonomy_index" = "Autonomy Index",
  "autonomy_dummy" = "Autonomy Dummy",
  "d106"  = "Less Severe Violence",
  "d107"  = "Severe Violence",
  "d108" = "Sexual Violence", 
  "d129" = "Afraid of Husband", 
  "v714" = "Currently Working",
  "v714a" = "Has Job, Absent",
  "v746" = "Earns more than husband",
  "v731" = "Worked last 12 months",
  "mv633a" = "Husband has STI", 
  "mv633b" = "Husband has other women", 
  "mv633d"= "Wife is Tired", 
  "mv634a" = "to get angry", 
  "mv634b"= "to withhold financial means", 
  "mv634c"= "Use force", 
  "mv634d" = "Sex with another woman", 
  "mv739" = "Own Earnings", 
  "mv743a" = "Healthcare", 
  "mv743b" = "Large Purchases", 
  "mv744a" = "Goes out without telling", 
  "mv744b"= "Neglects children", 
  "mv744c"= "Argues with Husband", 
  "mv744d" = "Refuses sex", 
  "mv744e" = "Burns food"
)

get_labels <- function(vars) {
  labels <- outcome_labels[vars]
  ifelse(is.na(labels), vars, labels)
}
# ============================================================
#  SURVEY DESIGNS
# ============================================================

# Men's dataset (subset to observations with a valid PSU)
data_men <- data_2019 %>% filter(!is.na(mv021))

mysurvey_women <- svydesign(
  id      = ~v021,
  strata  = ~v022,
  weights = ~wt_women,
  data    = data_2019,
  nest    = TRUE
)

mysurvey_men <- svydesign(
  id      = ~mv021,
  strata  = ~mv022,
  weights = ~wt_men,
  data    = data_men,
  nest    = TRUE
)

options(survey.lonely.psu = "adjust")  # handle single-PSU strata

# ============================================================
#  UNWEIGHTED GROUP COUNTS (for reference)
# ============================================================

group_counts_women <- data_2019 %>%
  filter(!is.na(combined_group)) %>%
  count(combined_group) %>%
  arrange(combined_group)

group_counts_men <- data_men %>%
  filter(!is.na(combined_group)) %>%
  count(combined_group) %>%
  arrange(combined_group)

# ============================================================
#  TABLE FORMATTING HELPER
# ============================================================

# landscape = TRUE  → max width 9 inches (use for 4+ outcome tables)
# landscape = FALSE → max width 6.5 inches (standard portrait)
fmt_tbl <- function(tbl, landscape = FALSE) {
  tbl %>%
    as_flex_table() %>%
    fit_to_width(max_width = ifelse(landscape, 9.0, 6.5)) %>%
    fontsize(size = 8, part = "all") %>%
    fontsize(size = 9, part = "header") %>%
    padding(padding = 3, part = "all") %>%
    set_table_properties(layout = "autofit")
}

# ============================================================
#  VARIABLE COMPLETENESS CHECK
# ============================================================

# Verify all required variables are present before analysis
all_vars <- c(controls_hh, controls_women, controls_men,
              outcomes_dec, outcomes_ipv, outcomes_work)

missing_vars <- setdiff(all_vars, names(data_2019))
if (length(missing_vars) > 0) {
  message("Missing variables: ", paste(missing_vars, collapse = ", "))
} else {
  message("All required variables present.")
}
############################################# END ###################################################
