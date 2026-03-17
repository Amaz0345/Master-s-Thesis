

library(flextable)
library(officer)
library(modelsummary)
# ============================================================
#  WOMEN'S DECISION-MAKING — NO CONTROLS
# ============================================================

outcomes_dec <- c("v739", "v743a", "v743b", "v743d", "v743f", "autonomy_index", "autonomy_dummy")
model_results_1 <- list()

for (i in outcomes_dec) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status * treated_district"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)
  },
  error = function(e) {
    message(paste("Error fitting model for", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_1[[i]] <- model_i
}
custom_labels_1 <- c("Own Earnings", "Own Healthcare", "Large Purchases", "Visiting Family", "Husband's Money", "Autonomy Index", "Autonomy Dummy")
outcomes_1 <- c("v739", "v743a", "v743b", "v743d", "v743f", "autonomy_index", "autonomy_dummy")


# ============================================================
#  IPV OUTCOMES — NO CONTROLS
# ============================================================

outcomes_ipv <- c("d106", "d107", "d108", "d129")
custom_2     <- c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")
model_results_3 <- list()

for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)
  },
  error = function(e) {
    message(paste("Error fitting model for", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_3[[i]] <- model_i
}


# ============================================================
#  MEN'S ATTITUDES: REFUSE SEX — NO CONTROLS
# ============================================================

outcomes_men_1  <- c("mv633a", "mv633b", "mv633d", "mv634a", "mv634b", "mv634c", "mv634d")
outcomes_men_1a <- c("mv633a", "mv633b", "mv633d", "mv634a")
outcomes_men_1b <- c("mv634b", "mv634c", "mv634d")
custom_men_1a   <- c("Refuse sex: husband has STI", "Refuse sex: husband has other women",
                     "Refuse sex: wife is tired", "Refuse sex: right to get angry")
custom_men_1b   <- c("Refuse sex: right to withhold financial means", "Refuse sex: right to use force",
                     "Refuse sex: right to have sex with another woman")

model_results_5 <- list()
for (i in outcomes_men_1) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_5[[i]] <- model_i
}

# ============================================================
#  MEN'S DECISION-MAKING — NO CONTROLS
# ============================================================

outcomes_men_2 <- c("mv739", "mv743a", "mv743b")
custom_men_2   <- c("Own Earnings", "Healthcare", "Large Purchases")
model_results_6 <- list()

for (i in outcomes_men_2) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_6[[i]] <- model_i
}

# ============================================================
#  MEN'S ATTITUDES: VIOLENCE JUSTIFIED — NO CONTROLS
# ============================================================

outcomes_men_3  <- c("mv744a", "mv744b", "mv744c", "mv744d", "mv744e")
outcomes_men_3a <- c("mv744a", "mv744b", "mv744c")
outcomes_men_3b <- c("mv744d", "mv744e")
custom_men_3a   <- c("Justified: wife goes out without telling", "Justified: wife neglects the children", "Justified: wife argues with husband")
custom_men_3b   <- c("Justified: wife refuses to have sex", "Justified: wife burns food")

model_results_7 <- list()
for (i in outcomes_men_3) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_7[[i]] <- model_i
}


# ============================================================
#  WOMEN'S ECONOMIC PARTICIPATION — NO CONTROLS
# ============================================================

model_results_12 <- list()

for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_12[[i]] <- model_i
}
# ============================================================
#  Make and export tables to word 
# ============================================================

make_fe_tbl <- function(models, title, col_names) {
  modelsummary(
    setNames(Filter(Negate(is.null), models), col_names),
    coef_map = c("eligibility_status:treated_district" = "Eligibility \u00d7 Treated"),
    gof_map  = c("nobs", "r.squared"),
    stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    title    = title,
    notes    = "Survey-Weighted DiD Results, without controls. Standard errors (shown in parantheses) clustered at PSU level. Only DiD interaction term shown.The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program participation.***, **, * indicate p < 0.01, p < 0.05, p < 0.1, respectively",
    output   = "flextable"
  )
}

tbl_dec_1 <- make_fe_tbl(
  model_results_1,
  "Results: Women's Decision-Making",
  c("Own Earnings", "Own Healthcare", "Large Purchases", 
    "Visiting Family", "Husband's Money", "Autonomy Index", "Autonomy Dummy")
)

tbl_ipv_1 <- make_fe_tbl(
  model_results_3,
  "Results: Intimate Partner Violence",
  c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")
)

tbl_work_1 <- make_fe_tbl(
  model_results_11,
  "Results: Women's Economic Participation",
  c("Currently Working", "Has Job Absent", "Earns More than Husband", "Worked Last 12 Months")
)

tbl_men_refuse_1 <- make_fe_tbl(
  model_results_5,
  "Results: Men's Attitudes — Right to Refuse Sex",
  c("Husband has STI", "Husband has other women", "Wife is tired", 
    "Right to get angry", "Withhold financial means", "Use force", "Sex with another woman")
)

tbl_men_dec_1 <- make_fe_tbl(
  model_results_6,
  "Results: Men's Decision-Making",
  c("Own Earnings", "Healthcare", "Large Purchases")
)

tbl_men_viol_1 <- make_fe_tbl(
  model_results_7,
  "Results: Men's Attitudes — Violence Justified",
  c("Goes out without telling", "Neglects children", 
    "Argues with husband", "Refuses sex", "Burns food")
)

# export all to one Word document
doc <- read_docx() %>%
  body_add_flextable(tbl_dec_1)    %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_ipv_1)    %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_work_1)   %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_men_refuse_1) %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_men_dec_1)    %>% body_end_section_portrait() %>%
  body_add_flextable(tbl_men_viol_1)   %>% body_end_section_portrait()

print(doc, target = file.path(output_dir, "regressions_nocontrols.docx"))
message("Saved: regressions_nocontrols.docx")

############################################# END ###################################################




