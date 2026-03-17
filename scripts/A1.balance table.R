
# ============================================================
# Master Thesis: Survey-Weighted Balance Table 
#  Pre-2011 Characteristics
# ============================================================

data_balance <- data_2019 %>%
  filter(b2_01 < 2011 | b2_02 < 2011) %>%  # note: OR not AND, depending on your intent
  mutate(
    balance_group = case_when(
      treated_district == 0 & eligibility_status == 0 ~ "Control × Non-eligible",
      treated_district == 0 & eligibility_status == 1 ~ "Control × Eligible",
      treated_district == 1 & eligibility_status == 0 ~ "Treatment × Non-eligible",
      treated_district == 1 & eligibility_status == 1 ~ "Treatment × Eligible"
    )
  )

balance_survey <- svydesign(
  id      = ~v021,
  strata  = ~v022,
  weights = ~wt_women,
  data    = data_balance,  
  nest    = TRUE
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
    statistic = list(all_continuous()  ~ "{mean}",
                     all_categorical() ~ "{p}%"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "svy.kruskal.test",   # works with 4 groups
      all_categorical() ~ "svy.chisq.test"  # chi-square for categorical with 4 groups
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Comparison Districts**",
    c(stat_3, stat_4) ~ "**Treatment Districts**"
  ) %>%
  modify_header(
    stat_1 ~ "**Control × Non-eligible**",
    stat_2 ~ "**Control × Eligible**",
    stat_3 ~ "**Treatment × Non-eligible**",
    stat_4 ~ "**Treatment × Eligible**"
  ) %>%
  modify_caption("**Table A1. Balance Table: Pre-2011 Characteristics**") %>%
  modify_footnote(
    all_stat_cols() ~ "Means shown for continuous variables, proportions for categorical. P-values from survey-weighted Kruskal–Wallis test and chi-square tests across all four groups. Survey design accounts for stratification and clustering at PSU level."
  )

# ── Exporting to LaTeX ────────────────────────────────────────

balance_table |> 
  as_gt() |> 
  gt::gtsave(filename = "outputs/balance_table.ltx")









