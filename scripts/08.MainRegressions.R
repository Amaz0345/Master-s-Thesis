
# ============================================================
#  Master Thesis: Survey Weighted Generalized Linear Models (Main Results)
#  Author: Ananya Mazumder
#  Export to Word via officer + flextable
# ============================================================

library(flextable)
library(officer)
library(modelsummary)
# ============================================================
#  WOMEN'S DECISION-MAKING — WITH CONTROLS
# ============================================================

model_results_2 <- list()
for (i in outcomes_dec) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", formula_controls_women))  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)
  },
  error = function(e) {
    message(paste("Error fitting model for", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_2[[i]] <- model_i
}


# ============================================================
#  IPV OUTCOMES — WITH CONTROLS
# ============================================================

model_results_4 <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", formula_controls_women))  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)
  },
  error = function(e) {
    message(paste("Something got messed up somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_4[[i]] <- model_i
}


# ============================================================
#  MEN'S ATTITUDES: REFUSE SEX — WITH CONTROLS
# ============================================================
controls_all_men  <- c(controls_men, controls_hh)
controls_formula  <- paste0(controls_all_men, collapse = " + ")

model_results_8 <- list()
for (i in outcomes_men_1) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", formula_controls_men))  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_8[[i]] <- model_i
}


# ============================================================
#  MEN'S DECISION-MAKING — WITH CONTROLS
# ============================================================

model_results_9 <- list()
for (i in outcomes_men_2) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", formula_controls_men))  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_9[[i]] <- model_i
}

# ============================================================
#  MEN'S ATTITUDES: VIOLENCE JUSTIFIED — WITH CONTROLS
# ============================================================

model_results_10 <- list()
for (i in outcomes_men_3) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", formula_controls_men))  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_men)
  },
  error = function(e) {
    message(paste("no; something is wrong somehow", i, ":", e$message))
    return(NULL)
  })
  if (!is.null(model_i)) model_results_10[[i]] <- model_i
}

# ============================================================
#  WOMEN'S ECONOMIC PARTICIPATION — WITH CONTROLS
# ============================================================

controls_women_1 <- c("v012", "v012_sq", "v133", "v437", "v438", "v511", "d113", "v201", "v715")
controls_all     <- c(controls_women_1, controls_hh)
controls_formula <- paste(c(controls_women_1, controls_hh), collapse = " + ")
model_results_12 <- list()

for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))
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
##Helper function ##
make_fe_tbl <- function(models, title, col_names) {
  modelsummary(
    setNames(Filter(Negate(is.null), models), col_names),
    coef_map = c("eligibility_status:treated_district" = "Eligibility \u00d7 Treated"),
    gof_map  = c("nobs", "r.squared"),
    stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    title    = title,
    notes    = "Survey-Weighted DiD Results, standard errors (shown in parantheses) clustered at PSU level. Only DiD interaction term shown.The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program participation.***, **, * indicate p < 0.01, p < 0.05, p < 0.1, respectively",
    output   = "flextable"
  )
}

tbl_dec <- make_fe_tbl(
  model_results_2,
  "Survey Weighted GLM: Women's Decision-Making",
  get_labels(outcomes_dec)
)

tbl_ipv <- make_fe_tbl(
  model_results_4,
  "Survey Weighted GLM: Intimate Partner Violence",
  get_labels(outcomes_ipv)
)

tbl_work <- make_fe_tbl(
  model_results_12,
  "Survey Weighted GLM: Women's Economic Participation",
  get_labels(outcomes_work)
)

tbl_men_refuse <- make_fe_tbl(
  model_results_8,
  "Survey Weighted GLM: Men's Attitudes — Right to Refuse Sex",
  get_labels(outcomes_men_1)
)

tbl_men_dec <- make_fe_tbl(
  model_results_9,
  "Survey Weighted GLM: Men's Decision-Making",
  get_labels(outcomes_men_2)
)

tbl_men_viol <- make_fe_tbl(
  model_results_10,
  "Survey Weighted GLM: Men's Attitudes — Violence Justified",
  get_labels(outcomes_men_3)
)

# export all to one Word document
doc <- read_docx() %>%
  body_add_flextable(tbl_dec)    %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_ipv)    %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_work)   %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_men_refuse) %>% body_end_section_landscape() %>%
  body_add_flextable(tbl_men_dec)    %>% body_end_section_portrait() %>%
  body_add_flextable(tbl_men_viol)   %>% body_end_section_portrait()

print(doc, target = file.path(output_dir, "regression_results.docx"))
message("Saved: regression_results.docx")

############################################# END ###################################################




