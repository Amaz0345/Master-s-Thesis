## Placebo test ##

# Filter to ineligible women
mysurvey_women_ineligible <- mysurvey_women %>%
  filter(eligible == 0)

# Same regression specifications as your main analysis
placebo_reg1 <- lm(outcome ~ treated_district + controls, 
                   data = mysurvey_women_ineligible)
