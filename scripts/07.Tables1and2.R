# ============================================================
#  Master Thesis: Descriptive Tables
#  Author: Ananya Mazumder 
#  Requires: setup.R to have been run first
# ============================================================

# ============================================================
#  TABLE 1: WOMEN & HOUSEHOLD CONTROLS
# ============================================================

desc_table_1 <- mysurvey_women %>%
  tbl_svysummary(
    include   = all_of(c(controls_women, controls_hh)),
    by        = combined_group,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 3,
    missing = "no",
    percent = "column"
  ) %>%
  modify_caption("Table 1. Survey-Weighted Descriptive Statistics: Women & Household") %>%
  modify_header(
    stat_1 ~ paste0("**Control × Ineligible**\nN = ",  group_counts_women$n[1]),
    stat_2 ~ paste0("**Treated × Ineligible**\nN = ",  group_counts_women$n[2]),
    stat_3 ~ paste0("**Control × Eligible**\nN = ",    group_counts_women$n[3]),
    stat_4 ~ paste0("**Treated × Eligible**\nN = ",    group_counts_women$n[4])
  ) %>%
  modify_footnote(
    all_stat_cols() ~ paste(
      "Mean (SD) for continuous variables; N (%) for categorical variables.",
      "All statistics account for complex survey design with sampling weights.",
      "N represents unweighted sample size.",
      "Wealth index: 1 = Poorest, 5 = Richest.",
      "Sex of household head: 1 = Male, 2 = Female.",
      "Binary variables: 0 = No, 1 = Yes, 7 = Not a dejure resident (where applicable). For all indicators based on the household members (PR file), either the de jure population (usual resident of the house) or the de facto population (slept at the home last night) is selected."
    )
  ) %>%
  fmt_tbl(landscape = TRUE)

# ============================================================
#  TABLE 2: MEN'S CONTROLS
# ============================================================

desc_table_2 <- mysurvey_men %>%
  tbl_svysummary(
    include   = all_of(controls_men),
    by        = combined_group,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 3,
    missing = "no",
    percent = "column"
  ) %>%
  modify_caption("Table 2. Survey-Weighted Descriptive Statistics: Men") %>%
  modify_header(
    stat_1 ~ paste0("**Control × Ineligible**\nN = ", group_counts_men$n[1]),
    stat_2 ~ paste0("**Treated × Ineligible**\nN = ", group_counts_men$n[2]),
    stat_3 ~ paste0("**Control × Eligible**\nN = ",   group_counts_men$n[3]),
    stat_4 ~ paste0("**Treated × Eligible**\nN = ",   group_counts_men$n[4])
  ) %>%
  modify_footnote(
    all_stat_cols() ~ paste(
      "Mean (SD) for continuous variables; N (%) for categorical variables.",
      "All statistics account for complex survey design with sampling weights.",
      "N represents unweighted sample size.",
      "Wealth index: 1 = Poorest, 5 = Richest.",
      "Binary variables: 0 = No, 1 = Yes."
    )
  ) %>%
  fmt_tbl(landscape = TRUE)

# ============================================================
#  EXPORT TO WORD
# ============================================================

doc_descriptives <- read_docx() %>%
  
  # Title page / section header
  body_add_par("Descriptive Statistics", style = "heading 1") %>%
  body_add_par(" ") %>%
  
  # Table 1 — Women & Household (landscape)
  body_add_par("Table 1. Women & Household Controls", style = "heading 2") %>%
  body_add_flextable(desc_table_1) %>%
  body_end_section_landscape() %>%
  
  # Table 2 — Men (landscape)
  body_add_par("Table 2. Men's Controls", style = "heading 2") %>%
  body_add_flextable(desc_table_2) %>%
  body_end_section_landscape()

print(doc_descriptives,
      target = file.path(output_dir, "descriptive_tables.docx"))

message("Saved: descriptive_tables.docx")

############################################# END ###################################################
