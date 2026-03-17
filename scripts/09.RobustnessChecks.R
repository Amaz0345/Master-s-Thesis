# ============================================================
#  Master Thesis: Robustness Checks 
#  Author: Ananya Mazumder
# ============================================================
# Run setup script first before this file


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


# ── Helper: run DiD for a given eligibility variable ─────────
# Returns a tidy dataframe of results across all autonomy outcomes
run_did <- function(data, eligibility_var, label) {
  
  results <- map_dfr(outcomes_dec, function(outcome) {
    
    formula_str <- paste0(
      outcome, " ~ ", eligibility_var, " * treated_district + ",
      formula_controls_women
    )
    
    # Update survey design with the new eligibility variable
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
      filter(str_detect(term, paste0(eligibility_var, ".*treated_district|treated_district.*", eligibility_var))) %>%
      mutate(outcome = outcome, spec = label)
  })
  
  return(results)
}


# ── Run all three eligibility specs ──────────────────────────
results_baseline <- run_did(data_2019, "eligibility_status",    "Baseline")
results_narrow   <- run_did(data_2019, "eligibility_narrow",    "Narrow window")
results_shifted  <- run_did(data_2019, "eligibility_shifted",   "Shifted control")
results_no_cutoff <- run_did(data_2019, "eligibility_no_cutoff","Drop age 21–22")

# Combine
results_eligibility <- bind_rows(
  results_baseline,
  results_narrow,
  results_shifted,
  results_no_cutoff
)

# ── Plot: coefficient comparison across specs ─────────────────
plot_eligibility <- results_eligibility %>%
  filter(outcome == "autonomy_index") %>%   # change outcome as needed
  ggplot(aes(x = spec, y = estimate, ymin = conf.low, ymax = conf.high, color = spec)) +
  geom_pointrange(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  labs(
    title    = "Robustness Check 1: Alternative Eligibility Windows",
    subtitle = "Outcome: Autonomy Index | DiD interaction coefficient",
    x        = NULL,
    y        = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(plot_eligibility)
ggsave(file.path(output_dir, "robustness_eligibility_windows.png"),
       plot_eligibility, width = 8, height = 5, dpi = 300)


# ============================================================
#  2. PLACEBO TREATMENT GROUPS
# ============================================================

# ── 2a. Pre-control-window placebo ───────────────────────────
# "Fake treated": children born 2000–2004 (should be ineligible, before program)
# "Fake control": children born 2005–2008 (real control group)
# If DiD is picking up a real program effect, this should give null results

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
# IGMSY was only for 1st and 2nd births — women with 3rd+ child
# born in the treatment window were NOT eligible
# Should get null results if the effect is program-specific

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


# ── Run placebo DiDs ──────────────────────────────────────────
results_placebo_pre <- run_did(data_2019, "eligibility_placebo_pre", "Placebo: pre-window")
results_placebo_3rd <- run_did(data_2019, "eligibility_placebo_3rd", "Placebo: 3rd child")

# Combine with baseline for comparison
results_placebo <- bind_rows(
  results_baseline,
  results_placebo_pre,
  results_placebo_3rd
)

# ── Plot: placebo vs baseline ─────────────────────────────────
plot_placebo <- results_placebo %>%
  filter(outcome == "autonomy_index") %>%
  ggplot(aes(x = spec, y = estimate, ymin = conf.low, ymax = conf.high, color = spec)) +
  geom_pointrange(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  labs(
    title    = "Robustness Check 2: Placebo Treatment Groups",
    subtitle = "Outcome: Autonomy Index | DiD interaction coefficient\nPlacebo specs should be near zero",
    x        = NULL,
    y        = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(plot_placebo)
ggsave(file.path(output_dir, "robustness_placebo.png"),
       plot_placebo, width = 8, height = 5, dpi = 300)




# ============================================================
#  IGMSY Thesis — Export Robustness Results to Word
#  Run after robustness_checks.R
# ============================================================
## Redefining the helper function ## 
# ── Helper: matches fmt_tbl() from setup.R ───────────────────
make_ft <- function(df, title) {
  df %>%
    select(Specification = spec,
           Outcome       = outcome,
           Coefficient   = estimate,
           `Std. Error`  = std.error,
           `p-value`     = p.value,
           `CI Lower`    = conf.low,
           `CI Upper`    = conf.high) %>%
    mutate(across(where(is.numeric), ~round(., 4))) %>%
    flextable() %>%
    set_caption(title) %>%
    fit_to_width(max_width = 9.0) %>%
    fontsize(size = 8, part = "all") %>%
    fontsize(size = 9, part = "header") %>%
    padding(padding = 3, part = "all") %>%
    set_table_properties(layout = "autofit")
}

# ── Build tables ─────────────────────────────────────────────
ft_eligibility <- make_ft(
  results_eligibility,
  "Table R1: Robustness Check 1 — Alternative Eligibility Windows (DiD Interaction Coefficient)"
)

ft_placebo <- make_ft(
  results_placebo,
  "Table R2: Robustness Check 2 — Placebo Treatment Groups (DiD Interaction Coefficient)"
)

# ── Export to Word document ────────────────────────────────────
doc <- read_docx() %>%
  
  body_add_par("Robustness Checks", style = "heading 1") %>%
  body_add_par(" ") %>%
  
  body_add_par("1. Alternative Eligibility Windows", style = "heading 2") %>%
  body_add_flextable(ft_eligibility) %>%
  body_end_section_landscape() %>%
  
  body_add_par("2. Placebo Treatment Groups", style = "heading 2") %>%
  body_add_flextable(ft_placebo) %>%
  body_end_section_landscape()

print(doc, target = file.path(output_dir, "robustness_checks.docx"))
message("Saved: robustness_checks.docx")
