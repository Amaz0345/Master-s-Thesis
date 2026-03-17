# ============================================================
#  Master Thesis: Robustness Checks for the Appendix
#  Author: Ananya Mazumder
# ============================================================
# Run setup script first before this file
# ============================================================


# ============================================================
#  1. ALTERNATIVE ELIGIBILITY WINDOWS
# ============================================================

# ── 1a. Narrower windows (tighter cohorts) ───────────────────
# Treated: 2013–2015 | Control: 2006–2008
data_2019 <- data_2019 %>%
  mutate(eligibility_narrow = case_when(
    v012 >= 21 &
      ((b2_01 >= 2013 & b2_01 <= 2015) |
         (b2_02 >= 2013 & b2_02 <= 2015 & !is.na(b2_02))) ~ 1,
    v012 >= 21 &
      ((b2_01 >= 2006 & b2_01 <= 2008) |
         (b2_02 >= 2006 & b2_02 <= 2008 & !is.na(b2_02))) ~ 0
  ))

# ── 1b. Shifted control window ───────────────────────────────
# Treated: 2012–2016 (unchanged) | Control: 2005–2007 (shorter)
data_2019 <- data_2019 %>%
  mutate(eligibility_shifted = case_when(
    v012 >= 21 &
      ((b2_01 >= 2012 & b2_01 <= 2016) |
         (b2_02 >= 2012 & b2_02 <= 2016 & !is.na(b2_02))) ~ 1,
    v012 >= 21 &
      ((b2_01 >= 2005 & b2_01 <= 2007) |
         (b2_02 >= 2005 & b2_02 <= 2007 & !is.na(b2_02))) ~ 0
  ))

# ── 1c. Drop women very close to age cutoff (21–22) ──────────
# Reduces misclassification from the age ≥21 rule
data_2019 <- data_2019 %>%
  mutate(eligibility_no_cutoff = case_when(
    v012 >= 23 &
      ((b2_01 >= 2012 & b2_01 <= 2016) |
         (b2_02 >= 2012 & b2_02 <= 2016 & !is.na(b2_02))) ~ 1,
    v012 >= 23 &
      ((b2_01 >= 2005 & b2_01 <= 2008) |
         (b2_02 >= 2005 & b2_02 <= 2008 & !is.na(b2_02))) ~ 0
  ))


# ============================================================
#  2. PLACEBO TREATMENT GROUPS
# ============================================================

# ── 2a. Pre-control-window placebo ───────────────────────────
# "Fake treated": children born 2000–2004 (before program, should be ineligible)
# "Fake control": children born 2005–2008 (real control group)
# Should give null results if the DiD is picking up a real program effect
data_2019 <- data_2019 %>%
  mutate(eligibility_placebo_pre = case_when(
    v012 >= 21 &
      ((b2_01 >= 2000 & b2_01 <= 2004) |
         (b2_02 >= 2000 & b2_02 <= 2004 & !is.na(b2_02))) ~ 1,  # fake "treated"
    v012 >= 21 &
      ((b2_01 >= 2005 & b2_01 <= 2008) |
         (b2_02 >= 2005 & b2_02 <= 2008 & !is.na(b2_02))) ~ 0   # real control
  ))

# ── 2b. Third-child placebo ───────────────────────────────────
# IGMSY covered 1st and 2nd births only; 3rd+ births in the treatment window
# were NOT eligible. Should give null results if the effect is program-specific.
data_2019 <- data_2019 %>%
  mutate(eligibility_placebo_3rd = case_when(
    v012 >= 21 & v201 >= 3 &
      ((b2_03 >= 2012 & b2_03 <= 2016) &
         (is.na(b2_01) | (b2_01 < 2012 | b2_01 > 2016)) &
         (is.na(b2_02) | (b2_02 < 2012 | b2_02 > 2016))) ~ 1,  # 3rd birth in window, 1st/2nd not
    v012 >= 21 &
      ((b2_01 >= 2005 & b2_01 <= 2008) |
         (b2_02 >= 2005 & b2_02 <= 2008 & !is.na(b2_02))) ~ 0   # real control
  ))


# ============================================================
#  HELPER FUNCTIONS
# ============================================================

# ── run_did(): DiD across a given set of outcomes ────────────
# Returns a tidy dataframe of interaction-term results
run_did <- function(data, eligibility_var, label, outcomes) {

  map_dfr(outcomes, function(outcome) {

    formula_str <- paste0(
      outcome, " ~ ", eligibility_var, " * treated_district + ",
      formula_controls_women
    )

    svy <- svydesign(
      id      = ~v021,
      strata  = ~v022,
      weights = ~wt_women,
      data    = data %>% filter(!is.na(.data[[eligibility_var]])),
      nest    = TRUE
    )

    fit <- tryCatch(
      svyglm(as.formula(formula_str), design = svy, family = gaussian()),
      error = function(e) NULL
    )

    if (is.null(fit)) return(NULL)

    tidy(fit, conf.int = TRUE) %>%
      filter(str_detect(term,
        paste0(eligibility_var, ".*treated_district|treated_district.*", eligibility_var)
      )) %>%
      mutate(outcome = get_labels(outcome), spec = label)
  })
}

# ── run_all_groups(): run_did() across all three outcome groups ──
# Returns a named list: $dec, $ipv, $work
run_all_groups <- function(data, eligibility_var, label) {
  list(
    dec  = run_did(data, eligibility_var, label, outcomes_dec),
    ipv  = run_did(data, eligibility_var, label, outcomes_ipv),
    work = run_did(data, eligibility_var, label, outcomes_work),
    men_1 = run_did(data, eligibility_var, label, outcomes_men_1),
    men_2 = run_did(data, eligibility_var, label, outcomes_men_2),
    men_3 = run_did(data, eligibility_var, label, outcomes_men_3)
  )
}


# ============================================================
#  RUN ROBUSTNESS CHECK 1: ALTERNATIVE ELIGIBILITY WINDOWS
# ============================================================

rc1_baseline   <- run_all_groups(data_2019, "eligibility_status",    "Baseline")
rc1_narrow     <- run_all_groups(data_2019, "eligibility_narrow",    "Narrow window")
rc1_shifted    <- run_all_groups(data_2019, "eligibility_shifted",   "Shifted control")
rc1_no_cutoff  <- run_all_groups(data_2019, "eligibility_no_cutoff", "Drop age 21–22")

# Stack results by outcome group
results_elig_dec  <- bind_rows(rc1_baseline$dec,  rc1_narrow$dec,  rc1_shifted$dec,  rc1_no_cutoff$dec)
results_elig_ipv  <- bind_rows(rc1_baseline$ipv,  rc1_narrow$ipv,  rc1_shifted$ipv,  rc1_no_cutoff$ipv)
results_elig_work <- bind_rows(rc1_baseline$work, rc1_narrow$work, rc1_shifted$work, rc1_no_cutoff$work)
results_elig_men_1 <- bind_rows(rc1_baseline$men_1, rc1_narrow$men_1, rc1_no_cutoff$men_1, rc1_shifted$men_1)
results_elig_men_2 <- bind_rows(rc1_baseline$men_2, rc1_narrow$men_2, rc1_no_cutoff$men_2, rc1_shifted$men_2)
results_elig_men_3 <- bind_rows(rc1_baseline$men_3, rc1_narrow$men_3, rc1_no_cutoff$men_3, rc1_shifted$men_3)


# ============================================================
#  RUN ROBUSTNESS CHECK 2: PLACEBO TREATMENT GROUPS
# ============================================================

rc2_baseline   <- run_all_groups(data_2019, "eligibility_status",       "Baseline")
rc2_placebo_pre <- run_all_groups(data_2019, "eligibility_placebo_pre", "Placebo: pre-window")
rc2_placebo_3rd <- run_all_groups(data_2019, "eligibility_placebo_3rd", "Placebo: 3rd child")

# Stack results by outcome group
results_plac_dec  <- bind_rows(rc2_baseline$dec,  rc2_placebo_pre$dec,  rc2_placebo_3rd$dec)
results_plac_ipv  <- bind_rows(rc2_baseline$ipv,  rc2_placebo_pre$ipv,  rc2_placebo_3rd$ipv)
results_plac_work <- bind_rows(rc2_baseline$work, rc2_placebo_pre$work, rc2_placebo_3rd$work)
results_plac_men_1 <- bind_rows(rc2_baseline$men_1, rc2_placebo_pre$men_1, rc2_placebo_3rd$men_1)
results_plac_men_2 <- bind_rows(rc2_baseline$men_2, rc2_placebo_pre$men_2, rc2_placebo_3rd$men_2)
results_plac_men_3 <- bind_rows(rc2_baseline$men_3, rc2_placebo_pre$men_3, rc2_placebo_3rd$men_3)



# ============================================================
#  EXPORT TO WORD
# ============================================================

# ── make_ft(): format a results dataframe as a flextable ─────
make_ft <- function(df, title) {
  df %>%
    select(
      Specification = spec,
      Outcome       = outcome,
      Coefficient   = estimate,
      `Std. Error`  = std.error,
      `p-value`     = p.value,
      `CI Lower`    = conf.low,
      `CI Upper`    = conf.high
    ) %>%
    mutate(across(where(is.numeric), ~round(., 4))) %>%
    flextable() %>%
    set_caption(title) %>%
    fit_to_width(max_width = 9.0) %>%
    fontsize(size = 8, part = "all") %>%
    fontsize(size = 9, part = "header") %>%
    padding(padding = 3, part = "all") %>%
    set_table_properties(layout = "autofit")
}

# ── Build all six tables ──────────────────────────────────────
ft_elig_dec  <- make_ft(results_elig_dec,  "Table A8a: Alternative Eligibility Windows — Decision-Making Outcomes")
ft_elig_ipv  <- make_ft(results_elig_ipv,  "Table A8b: Alternative Eligibility Windows — IPV Outcomes")
ft_elig_work <- make_ft(results_elig_work, "Table A8c: Alternative Eligibility Windows — Work Outcomes")
ft_elig_men_1 <- make_ft(results_elig_men_1, "Table A8d: Alternative Eligibility Windows — Men's Attitudes 1")
ft_elig_men_2 <- make_ft(results_elig_men_2, "Table A8e: Alternative Eligibility Windows — Men's Attitudes 2")
ft_elig_men_3 <- make_ft(results_elig_men_3, "Table A8f: Alternative Eligibility Windows — Men's Attitudes 3")


ft_plac_dec  <- make_ft(results_plac_dec,  "Table A9a: Placebo Treatment Groups — Decision-Making Outcomes")
ft_plac_ipv  <- make_ft(results_plac_ipv,  "Table A9b: Placebo Treatment Groups — IPV Outcomes")
ft_plac_work <- make_ft(results_plac_work, "Table A9c: Placebo Treatment Groups — Work Outcomes")
ft_plac_men_1 <- make_ft(results_plac_men_1, "Table A9d: Placebo Treatment Groups - Men's Attitudes 1")
ft_plac_men_2 <- make_ft(results_plac_men_2, "Table A9e: Placebo Treatment Groups - Men's Attitudes 2")
ft_plac_men_3 <- make_ft(results_plac_men_3, "Table A9f: Placebo Treatment Groups - Men's Attitudes 3")


# ── Write to Word ─────────────────────────────────────────────
doc <- read_docx() %>%

  body_add_par("Robustness Checks", style = "heading 1") %>%
  body_add_par(" ") %>%

  # Check 1: Alternative eligibility windows
  body_add_par("1. Alternative Eligibility Windows", style = "heading 2") %>%
  body_add_flextable(ft_elig_dec)  %>% body_end_section_landscape() %>%
  body_add_flextable(ft_elig_ipv)  %>% body_end_section_landscape() %>%
  body_add_flextable(ft_elig_work) %>% body_end_section_landscape() %>%
  body_add_flextable(ft_elig_men_1) %>% body_end_section_landscape() %>%
  body_add_flextable(ft_elig_men_2) %>% body_end_section_landscape() %>%
  body_add_flextable(ft_elig_men_3) %>% body_end_section_landscape() %>%
  
  # Check 2: Placebo treatment groups
  body_add_par("2. Placebo Treatment Groups", style = "heading 2") %>%
  body_add_flextable(ft_plac_dec)  %>% body_end_section_landscape() %>%
  body_add_flextable(ft_plac_ipv)  %>% body_end_section_landscape() %>%
  body_add_flextable(ft_plac_work) %>% body_end_section_landscape() %>% 
  body_add_flextable(ft_plac_men_1) %>% body_end_section_landscape() %>%
  body_add_flextable(ft_plac_men_2) %>% body_end_section_landscape() %>%
  body_add_flextable(ft_plac_men_3) %>% body_end_section_landscape() 
  
print(doc, target = file.path(output_dir, "appendix_robustness_checks.docx"))
message("Saved: appendix_robustness_checks.docx")


# ============================================================
#  EXPORT TO LaTeX
# ============================================================
# Produces a standalone .tex file you can \input{} into your
# thesis appendix. Requires: knitr, kableExtra
# In your thesis preamble, make sure you have:
#   \usepackage{booktabs}
#   \usepackage{longtable}
#   \usepackage{caption}
#   \usepackage{lscape}   % or pdflscape for rotated pages
# ============================================================

library(knitr)
library(kableExtra)

# ── make_latex_tbl(): format a results dataframe as a LaTeX table ──
make_latex_tbl <- function(df, caption, label) {
  df %>%
    select(
      Specification = spec,
      Outcome       = outcome,
      Coefficient   = estimate,
      `Std. Error`  = std.error,
      `$p$-value`   = p.value,
      `CI Lower`    = conf.low,
      `CI Upper`    = conf.high
    ) %>%
    mutate(across(where(is.numeric), ~formatC(., digits = 4, format = "f"))) %>%
    kbl(
      format    = "latex",
      booktabs  = TRUE,
      caption   = caption,
      label     = label,
      linesep   = "",          # suppress extra space every 5 rows
      align     = "llrrrrr"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down"),
      font_size     = 8
    ) %>%
    row_spec(0, bold = TRUE)   # bold header row
}

# ── Build all LaTeX tables ────────────────────────────────
lt_elig_dec  <- make_latex_tbl(results_elig_dec,  
  caption = "Robustness Check 1: Alternative Eligibility Windows --- Decision-Making Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc1_dec")

lt_elig_ipv  <- make_latex_tbl(results_elig_ipv,  
  caption = "Robustness Check 1: Alternative Eligibility Windows --- IPV Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc1_ipv")

lt_elig_work <- make_latex_tbl(results_elig_work, 
  caption = "Robustness Check 1: Alternative Eligibility Windows --- Work Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc1_work")

lt_elig_men_1 <- make_latex_tbl(results_elig_men_1,
  caption = "Robustness Check 1: Alternative Eligibility Windows --- Men Attitudes 1",
  label = "tab: rc1_men_1")

lt_elig_men_2 <- make_latex_tbl(results_elig_men_2,
  caption = "Robustness Check 1: Alternative Eligibility Windows --- Men Attitudes 2",
  label = "tab:rc1_men_2")

lt_elig_men_3 = make_latex_tbl(results_elig_men_3,
  caption = "Robustness Check 1: Alternative Eligibility Windows --- Men Attitudes 3",
  label = "tab:rc1_men_3")

lt_plac_dec  <- make_latex_tbl(results_plac_dec,  
  caption = "Robustness Check 2: Placebo Treatment Groups --- Decision-Making Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc2_dec")

lt_plac_ipv  <- make_latex_tbl(results_plac_ipv,  
  caption = "Robustness Check 2: Placebo Treatment Groups --- IPV Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc2_ipv")

lt_plac_work <- make_latex_tbl(results_plac_work, 
  caption = "Robustness Check 2: Placebo Treatment Groups --- Work Outcomes (DiD Interaction Coefficient)",
  label   = "tab:rc2_work")

lt_plac_men_1 <- make_latex_tbl(results_plac_men_1,
  caption = "Robustness Check 2: Placebo Treatment Groups --- Men's Attitudes 1",
  label = "tab:rc2_men_1")
lt_plac_men_2 <- make_latex_tbl(results_plac_men_2,
   caption = "Robustness Check 2: Placebo Treatment Groups --- Men's Attitudes 2",
   label = "tab:rc2_men_2")
lt_plac_men_3 <- make_latex_tbl(results_plac_men_3,
   caption = "Robustness Check 2: Placebo Treatment Groups --- Men's Attitudes 1",
   label = "tab:rc2_men_3")




# ── Wrap each table in a landscape environment ───────────────
# Uses pdflscape so the page itself rotates in the PDF viewer
wrap_landscape <- function(tbl) {
  paste0(
    "\\begin{landscape}\n",
    tbl,
    "\n\\end{landscape}\n"
  )
}

# ── Assemble and write the .tex file ─────────────────────────
# Each table gets its own page. The file is self-contained
# but is designed to be \input{} into your thesis appendix 
# (it does NOT include \documentclass or \begin{document}).

latex_body <- paste(
  "% ============================================================",
  "% Appendix: Robustness Checks",
  "% Auto-generated — do not edit by hand",
  "% ============================================================",
  "",
  "\\section*{Robustness Checks}",
  "",
  "\\subsection*{1. Alternative Eligibility Windows}",
  wrap_landscape(lt_elig_dec),
  "\\clearpage",
  wrap_landscape(lt_elig_ipv),
  "\\clearpage",
  wrap_landscape(lt_elig_work),
  "\\clearpage",
  wrap_landscape(lt_elig_men_1),
  "\\clearpage",
  wrap_landscape(lt_elig_men_2),
  "\\clearpage",
  wrap_landscape(lt_elig_men_3), 
  "",
  "\\subsection*{2. Placebo Treatment Groups}",
  wrap_landscape(lt_plac_dec),
  "\\clearpage",
  wrap_landscape(lt_plac_ipv),
  "\\clearpage",
  wrap_landscape(lt_plac_work),
  "\\clearpage",
  wrap_landscape(lt_plac_men_1),
  "\\clearpage",
  wrap_landscape(lt_plac_men_2),
  "\\clearpage",
  wrap_landscape(lt_plac_men_3),
  "\\clearpage",
  sep = "\n"
)

writeLines(latex_body, file.path(output_dir, "appendix_robustness_checks.tex"))
message("Saved: appendix_robustness_checks.tex")
