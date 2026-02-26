
##balancing test t-test: between control and treated districts (reporting mean and SD) sample is for women who delivered in pre intervention period (prior to 2011)
# Filtering the data for the balance test 
# 1. Filter the raw data
data_balance <- data_2019 %>%
  filter(
    b2_01 < 2011, 
    b2_02 < 2011
  )

# 2. Re-create the survey design for this specific subset
balance_survey <- svydesign(
  id = ~v021,
  strata = ~v022, 
  weights = ~wt_women,
  data = data_2019,
  nest = TRUE
) 
options(survey.lonely.psu = "adjust")

# Ensure treatment_status is a factor for the 'by' argument

balance_table <- balance_survey %>%
  tbl_svysummary(
    include = c(all_of(outcomes_dec), 
                all_of(outcomes_work), 
                all_of(outcomes_ipv), 
                all_of(controls_hh), 
                all_of(controls_women)),
    by = treated_district, # Compare treatment vs control
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no", 
    test = list(all_continuous() ~ "svy.t.test",
                all_categorical() ~ "svy.wald.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 3. Balance Test: Comparison of Means (Pre-2011 Births Only)**")

balance_table %>% as_gt() ##taking forever to run ugh


library(gtsummary)
library(dplyr)
library(gt)

# Create survey design
balance_survey <- svydesign(
  id = ~v021,
  strata = ~v022, 
  weights = ~wt_women,
  data = data_2019,
  nest = TRUE
) 

options(survey.lonely.psu = "adjust")

# Create combined grouping variable (4 groups instead of 2)
data_2019 <- data_2019 %>%
  mutate(
    balance_group = case_when(
      treated_district == 0 & eligibility_status == 0 ~ "Control × Non-eligible",
      treated_district == 0 & eligibility_status == 1 ~ "Control × Eligible",
      treated_district == 1 & eligibility_status == 0 ~ "Treatment × Non-eligible",
      treated_district == 1 & eligibility_status == 1 ~ "Treatment × Eligible"
    )
  )

# Recreate survey design with new grouping
balance_survey <- svydesign(
  id = ~v021,
  strata = ~v022, 
  weights = ~wt_women,
  data = data_2019,
  nest = TRUE
)

# Create the table
balance_table <- balance_survey %>%
  tbl_svysummary(
    include = c(all_of(outcomes_dec), 
                all_of(outcomes_work), 
                all_of(outcomes_ipv), 
                all_of(controls_hh), 
                all_of(controls_women)),
    by = balance_group,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "svy.t.test",
      all_categorical() ~ "svy.t.test"
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
  ) %>%
  modify_header(
    stat_1 ~ "**Eligible cohort**",
    stat_2 ~ "**Non-eligible cohort**",
    stat_3 ~ "**Eligible cohort**",
    stat_4 ~ "**Non-eligible cohort**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Comparison districts**",
    c(stat_3, stat_4) ~ "**Treatment districts**"
  ) %>%
  modify_caption("**Table S1.1. Balance between Eligible and Non-eligible Cohorts**") %>%
  modify_footnote(
    all_stat_cols() ~ "Means shown for all variables. P-values adjusted for district-level clustering."
  )

balance_table %>% as_gt()



# 1. Create Table for Control Districts (treated_district == 0)
tab_control <- balance_survey %>%
  subset(treated_district == 0) %>%
  tbl_svysummary(
    by = balance_group, # This should now only have 2 levels (Eligible/Non-eligible)
    include = c(all_of(outcomes_dec), all_of(outcomes_work), 
                all_of(outcomes_ipv), all_of(controls_hh), 
                all_of(controls_women)),
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test", 
                    all_categorical() ~ "svy.chisq.test"))

# 2. Create Table for Treated Districts (treated_district == 1)
tab_treated <- balance_survey %>%
  subset(treated_district == 1) %>%
  tbl_svysummary(
    by = combined_group,
    include = c(all_of(outcomes_dec), all_of(outcomes_work), 
                all_of(outcomes_ipv), all_of(controls_hh), 
                all_of(controls_women)),
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test", 
                    all_categorical() ~ "svy.chisq.test"))

# 3. Merge them into one master balance table
balance_table <- tbl_merge(
  tbls = list(tab_control, tab_treated),
  tab_spanner = c("**Control Districts**", "**Treatment Districts**")
) %>%
  modify_caption("**Table S1.1. Balance between Eligible and Non-eligible Cohorts**")

balance_table %>%
  as_flex_table() %>%
  save_as_docx(path = "balance_table_A4.docx")


tab_control <- balance_survey %>%
  subset(treated_district == 0) %>%
  tbl_svysummary(
    by = balance_group,
    include = c(all_of(outcomes_dec), all_of(outcomes_work), 
                all_of(outcomes_ipv), all_of(controls_hh), 
                all_of(controls_women)),
    label = var_labels,  
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test", 
                    all_categorical() ~ "svy.chisq.test"))

tab_treated <- balance_survey %>%
  subset(treated_district == 1) %>%
  tbl_svysummary(
    by = balance_group,
    include = c(all_of(outcomes_dec), all_of(outcomes_work), 
                all_of(outcomes_ipv), all_of(controls_hh), 
                all_of(controls_women)),
    label = var_labels,  
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test", 
                    all_categorical() ~ "svy.chisq.test"))

balance_table <- tbl_merge(
  tbls = list(tab_control, tab_treated),
  tab_spanner = c("**Control Districts**", "**Treatment Districts**")
) %>%
  modify_caption("**Table S1.1. Balance between Eligible and Non-eligible Cohorts**")

balance_table %>%
  as_flex_table() %>%
  save_as_docx(path = "balance_table_A4.docx")










