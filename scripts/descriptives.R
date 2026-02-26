## Descriptive tables ##
desc_table_1 <- mysurvey_women %>%
  tbl_svysummary(
    include = all_of(c(controls_women, controls_hh)),
    by = combined_group,
    statistic = list(
      all_continuous() ~ "{mean}\n({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 3,
    missing = "no"
  ) %>%
  modify_caption("**Table 1. Survey-Weighted Descriptives for Women & Household**") %>%
  modify_header(
    stat_1 ~ paste0("**Control District × Ineligible**  \nN = ", group_counts$n[1]),
    stat_2 ~ paste0("**Treated District × Ineligible**  \nN = ", group_counts$n[2]), 
    stat_3 ~ paste0("**Control District × Eligible**  \nN = ", group_counts$n[3]),
    stat_4 ~ paste0("**Treated District × Eligible**  \nN = ", group_counts$n[4])
  ) %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) for continuous variables; N (%) for categorical variables. N represents unweighted sample size.Wealth indices: 1 = Poorest, 5 = Richest. Sex of HH head: 1 = Male, 2 = Female. Binary variables: 0 = No, 1 = Yes, 7 = Not a dejure resident (where applicable). For all indicators based on the household members (PR file), either the de jure population (hv102 = 1) or the de facto population (hv103 = 1) is selected.")

desc_table_1 %>% as_gt()


# Create survey-specific-specific descriptives table for men

desc_table_2 <- mysurvey_men %>%
  tbl_svysummary(
    include = all_of(controls_men),
    by = combined_group,
    statistic =list(
      all_continuous() ~ "{mean}\n({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 3,
    missing = "no"
  ) %>%
  add_n() %>% 
  modify_caption("**Table 2. Survey-Weighted Descriptives for Men**") %>%
  modify_header(
    stat_1 ~ paste0("**Control District × Ineligible**  \nN = ", group_counts$n[1]),
    stat_2 ~ paste0("**Treated District × Ineligible**  \nN = ", group_counts$n[2]), 
    stat_3 ~ paste0("**Control District × Eligible**  \nN = ", group_counts$n[3]),
    stat_4 ~ paste0("**Treated District × Eligible**  \nN = ", group_counts$n[4])
  ) %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD) for continuous variables; N (%) for categorical variables. 0 = no, 1 = yes for binary variables.")
desc_table_2 %>% as_gt() 
