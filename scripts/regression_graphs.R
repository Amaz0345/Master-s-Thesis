
# Graph 1: Survey Weighted DiD Results for Decision-Making 
coef_data <- map_df(names(model_results_1), function(outcome) {
  tidy(model_results_1[[outcome]], conf.int = TRUE) %>%
    mutate(outcome = outcome)
}) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_1),
                           labels = custom_labels),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    )
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig.1 Survey-Weighted DiD Results: Treatment Effects on Decision-Making",
    subtitle = "No Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("outputs", "plot1.png"), width = 10, height = 6, dpi = 300)



# Graph 2:Survey Weighted DiD Results for Decision-Making (+controls)

coef_data <- map_df(names(model_results_2), function(outcome_name) {
  tidy(model_results_2[[outcome_name]], conf.int = TRUE) %>%
    mutate(outcome = outcome_name)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_2),
                           labels = custom_labels),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 2 Survey-Weighted DiD Results: Treatment Effects on Decision-Making",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("outputs", "plot2.png"), width = 10, height = 6, dpi = 300)


## Graph 3: Violence (IPV and attitudes towards IPV)

coef_data <- map_df(names(model_results_3), function(outcome_3) {
  tidy(model_results_3[[outcome_3]], conf.int = TRUE) %>%
    mutate(outcome = outcome_3)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_3),
                           labels = custom_2),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )


# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 3 Survey-Weighted DiD Results: Treatment Effects on Violence",
    subtitle = "without Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot3.png"), width = 10, height = 6, dpi = 300)


##with controls 

coef_data <- map_df(names(model_results_4), function(outcome_4) {
  tidy(model_results_4[[outcome_4]], conf.int = TRUE) %>%
    mutate(outcome = outcome_4)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_4),
                           labels = custom_2),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 4 Survey-Weighted DiD Results: Treatment Effects on Violence",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot4.png"), width = 10, height = 6, dpi = 300)


##Graph 5: Men's Attitudes towards decision-making, sex, violence 
## Justified: refusing sex? without controls 
coef_data <- map_df(names(model_results_5), function(outcome_5) {
  tidy(model_results_5[[outcome_5]], conf.int = TRUE) %>%
    mutate(outcome = outcome_5)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_5),
                           labels = custom_men_1),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 5 Survey-Weighted DiD Results: Are women justified in refusing sex?",
    subtitle = "without Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot5.png"), width = 10, height = 6, dpi = 300)

##with controls 

coef_data <- map_df(names(model_results_8), function(outcome_8) {
  tidy(model_results_8[[outcome_8]], conf.int = TRUE) %>%
    mutate(outcome = outcome_8)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_8),
                           labels = custom_men_1),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 6 Survey-Weighted DiD Results: Are women justified in refusing sex?",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot6.png"), width = 10, height = 6, dpi = 300)

## men's attitude towards decision-making (without controls)

coef_data <- map_df(names(model_results_6), function(outcome_6) {
  tidy(model_results_6[[outcome_6]], conf.int = TRUE) %>%
    mutate(outcome = outcome_6)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_6),
                           labels = custom_men_2),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 7 Survey-Weighted DiD Results: Who takes decisions?",
    subtitle = "without Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot7.png"), width = 10, height = 6, dpi = 300)

### men's attitude towards decision-making (with controls)

coef_data <- map_df(names(model_results_9), function(outcome_9) {
  tidy(model_results_9[[outcome_9]], conf.int = TRUE) %>%
    mutate(outcome = outcome_9)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_9),
                           labels = custom_men_2),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 8 Survey-Weighted DiD Results: Who takes decisions?",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot8.png"), width = 10, height = 6, dpi = 300)


##men's attitudes towards violence (without controls)
coef_data <- map_df(names(model_results_7), function(outcome_7) {
  tidy(model_results_7[[outcome_7]], conf.int = TRUE) %>%
    mutate(outcome = outcome_7)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_7),
                           labels = custom_men_3),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 9 Survey-Weighted DiD Results: Violence against wife justified?",
    subtitle = "without Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot9.png"), width = 10, height = 6, dpi = 300)

##men's attitudes towards violence (with controls)
coef_data <- map_df(names(model_results_10), function(outcome_10) {
  tidy(model_results_10[[outcome_10]], conf.int = TRUE) %>%
    mutate(outcome = outcome_10)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_10),
                           labels = custom_men_3),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 10 Survey-Weighted DiD Results: Violence against wife justified?",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot10.png"), width = 10, height = 6, dpi = 300)

##economic empowerment of women: women and work
coef_data <- map_df(names(model_results_11), function(outcome_11) {
  tidy(model_results_11[[outcome_11]], conf.int = TRUE) %>%
    mutate(outcome = outcome_11)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_11),
                           labels = custom_3),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 11 Survey-Weighted DiD Results: Economic Empowerment",
    subtitle = "without Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot11.png"), width = 10, height = 6, dpi = 300)


##women and work, with controls 

coef_data <- map_df(names(model_results_12), function(outcome_12) {
  tidy(model_results_12[[outcome_12]], conf.int = TRUE) %>%
    mutate(outcome = outcome_12)  
}) %>%
  filter(term %in% c("eligibility_status", 
                     "treated_district", 
                     "eligibility_status:treated_district")) %>%
  mutate(
    outcome_label = factor(outcome, 
                           levels = names(model_results_12),
                           labels = custom_3),
    term_label = case_when(
      term == "eligibility_status" ~ "Eligibility Status",
      term == "treated_district" ~ "Treated District",
      term == "eligibility_status:treated_district" ~ "Eligibility × Treated",
      TRUE ~ term
    ),
    term_label = factor(term_label, 
                        levels = c("Eligibility Status", 
                                   "Treated District", 
                                   "Eligibility × Treated"))
  )

# Create plot
ggplot(coef_data, aes(x = estimate, y = term_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Fig. 12 Survey-Weighted DiD Results: Economic Empowerment",
    subtitle = "with Controls",
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Outcome"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("outputs", "plot12.png"), width = 10, height = 6, dpi = 300)

##THE END 






