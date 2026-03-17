# ============================================================
#  Master Thesis: Fixed Effects OLS with Clusters 
#  Export to Word via officer + flextable
# ============================================================


#install.packages("fixest")
library(fixest)
library(flextable)
library(officer)
library(modelsummary)

# ============================================================
#  WOMEN'S DECISION-MAKING (FE OLS) — WITH CONTROLS
# ============================================================

fe_results_dec <- list()
for (i in outcomes_dec) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_dec[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}


# ============================================================
#  IPV OUTCOMES  (FE OLS) — WITH CONTROLS
# ============================================================
fe_results_ipv <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_ipv[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}



# ============================================================
#  MEN'S ATTITUDES: REFUSE SEX (FE OLS) — CONTROLS
# ============================================================
fe_results_men_1 <- list()
for (i in outcomes_men_1) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_men_1[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}

# ============================================================
# MEN'S DECISION-MAKING (FE OLS) — CONTROLS
# ============================================================

fe_results_men_2 <- list()
for (i in outcomes_men_2) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_men_2[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}

# ============================================================
#  MEN'S ATTITUDES: VIOLENCE JUSTIFIED — WITH CONTROLS
# ============================================================

fe_results_men_3 <- list()
for (i in outcomes_men_3) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_men_3[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}
# ============================================================
#  WOMEN'S ECONOMIC PARTICIPATION — WITH CONTROLS
# ============================================================
controls_women_1 <- c("v012", "v012_sq", "v133", "v437", "v438", "v511", "d113", "v201", "v715")
controls_all     <- c(controls_women_1, controls_hh)
controls_formula <- paste(c(controls_women_1, controls_hh), collapse = " + ")

fe_results_work <- list()
for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status:treated_district + ", 
                                 formula_controls_women, " | shdist"))
  fe_results_work[[i]] <- tryCatch({
    feols(formula_i,
          data    = data_2019,
          weights = ~wt_women,
          cluster = ~shdist)
  }, error = function(e) {
    message(paste("FE error for", i, ":", e$message))
    return(NULL)
  })
}



# ── Formatting helper ────────────────────────────────────────
# helper to build each modelsummary table in LaTeX

make_fe_latex <- function(models, title, col_names) {
  modelsummary(
    setNames(Filter(Negate(is.null), models), col_names),
    coef_map  = c("eligibility_status:treated_district" = "Eligibility x Treated"),
    gof_map   = c("nobs", "r.squared"),
    stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    title     = title,
    output    = "latex"
  ) %>% as.character()
}

tbl_fe_dec <- make_fe_latex(
  fe_results_dec,
  "FE OLS Results: Women's Decision-Making",
  get_labels(outcomes_dec)
)

tbl_fe_ipv <- make_fe_latex(
  fe_results_ipv,
  "FE OLS Results: Intimate Partner Violence",
  get_labels(outcomes_ipv)
)

tbl_fe_work <- make_fe_latex(
  fe_results_work,
  "FE OLS Results: Women's Economic Participation",
  get_labels(outcomes_work)
)

tbl_fe_men_1 <- make_fe_latex(
  fe_results_men_1,
  "FE OLS Results: Men's Attitudes --- Right to Refuse Sex",
  get_labels(outcomes_men_1)
)

tbl_fe_men_2 <- make_fe_latex(
  fe_results_men_2,
  "FE OLS Results: Men's Decision-Making",
  get_labels(outcomes_men_2)
)

tbl_fe_men_3 <- make_fe_latex(
  fe_results_men_3,
  "FE OLS Results: Men's Attitudes --- Violence Justified",
  get_labels(outcomes_men_3)
)

# ── Write all to one .tex file ───────────────────────────────
output_path <- file.path(output_dir, "fe_results.tex")

writeLines(
  c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{pdflscape}",
    "\\usepackage{geometry}",
    "\\usepackage{caption}",
    "\\captionsetup[table]{labelformat=empty}",
    "\\geometry{a4paper, margin=2cm}",
    "\\begin{document}",
    "",
    "\\begin{landscape}",
    tbl_fe_dec,
    tbl_fe_ipv,
    tbl_fe_work,
    tbl_fe_men_1,
    "\\end{landscape}",
    "",
    tbl_fe_men_2,
    tbl_fe_men_3,
    "",
    "\\end{document}"
  ),
  con = output_path
)

message("Saved: fe_results.tex")

##END## 



