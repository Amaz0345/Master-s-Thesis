rm(list = ls())

##think about setting up a data management structure e.g. a folder for ouputs, and then a link to a dropbox/github for access to the actual dataset so that you won't have to email the dataset too
## dataset is a once married woman's sample

library(haven)
library(ggplot2)
library(tidyverse)
library(stargazer)
library(survey)
library(gtsummary)
library(xfun)
library(broom)
library(here)
library(labelled)

data_2019 <- read_stata(here("data", "2019_final.dta"))
## data cleaning and prep ##
data_2019 <- data_2019 %>%
  mutate(
    v131 = to_factor(v131),
    v131 = case_when(
      v131 %in% c("caste", "tribe", "no caste / tribe", "don't know") ~ v131,
      TRUE ~ factor(NA, levels = levels(v131))    
      ) %>%
      set_variable_labels(v131 = "ethnicity")
  )


table(data_2019$v131, useNA = "always") ##checking to see it worked 

data_2019 <- data_2019 %>%
  mutate(
    v437 = case_when(
      v437 < 9000 ~ v437 / 10,
      TRUE ~ NA_real_
    ),
    v438 = case_when(
      v438 < 9000 ~ v438 / 10,
      TRUE ~ NA_real_
    )
  ) %>%
  set_variable_labels(
    v437 = "Weight (kg)",
    v438 = "Height (cm)"
  )

var_labels <- data_2019 %>% 
  map(~attr(., "label")) %>% 
  compact()

data_2019 <- data_2019 %>%
  mutate(across(everything(), ~{
    if (is.labelled(.)) {
      lbls <- val_labels(.)
      return(if_else(. >= 95 & . %in% lbls, NA_real_, as.numeric(.)))
    }
    return(.)
  })) %>%
  unlabelled() %>%
  mutate(across(where(is.numeric), ~zap_labels(.)))



##Constructing treated_district 
data_2019 <- data_2019 %>% 
  mutate(treated_district = ifelse(
    shdist %in% c(214, 367, 273, 220, 169, 930, 871, 122, 130, 291, 
                  825, 302, 287, 328, 339, 248, 60, 357, 242, 842, 
                  412, 69, 470, 36, 321, 847, 488, 900, 562, 49, 
                  581, 546, 28, 593, 585), 1, 0
  ))


## Construction of eligible versus ineligible women ##
# eligible woman is above 19, gave birth to either 1st or 2nd child between 2012-2016 #or 2016?
# firstly, drop women below 19 or 21? because if they need to be at least 19 in 2016 then they should be 21 in 2019


##creating eligibility and ineligibility status 
data_2019 <- data_2019 %>% 
  mutate(eligibility_status = case_when(
    # Eligible women (first or second child born 2012-2016)
    v012 >= 21  & 
      ((b2_01 >= 2012 & b2_01 <= 2016) | 
         (b2_02 >= 2012 & b2_02 <= 2016 & !is.na(b2_02))) ~ 1,
    
    # Ineligible - wrong birth year (2005-2009)
    v012 >= 21  &
      ((b2_01 >= 2005 & b2_01 < 2009) | 
         (b2_02 >= 2005 & b2_02 < 2009 & !is.na(b2_02))) ~ 0,
  ))
# 1 = eligible (2012-2016 births)
# 0 = ineligible (2005-2009 births)

data_2019 <- data_2019 %>% ##restoring variable labels 
  set_variable_labels(.labels = var_labels, .strict = FALSE)

##creating the sampling weight variable for women 
data_2019$wt_women <- data_2019$v005/1000000

#creating the sampling weight variable for men
data_2019$wt_men <- data_2019$mv005/1000000
##setting survey design for women: weight = v005, PSU= v021, strata =v022)  
##to identify survey design, need the 3 above variables
##creating this variable before setting variable design (will need it later)
# Add the combined grouping variable to the original data
##creating some variables before the survey packages run
data_2019 <- data_2019 %>%
  mutate(
    combined_group = interaction(
      haven::as_factor(eligibility_status), 
      haven::as_factor(treated_district), 
      sep = " × "
    )
  )
data_2019$v012_sq <- data_2019$v012^2
attr(data_2019$v012_sq, "label") <- "Square of Women's Age"

controls_hh <- c("v131","v136","v137","v151","v152","v119","v120", "v121", "v122", "v123", "v124", "v125", "mv190", "sm190s")
controls_women <- c("v012", "v012_sq", "v133","v437", "v438", "v511", "d113", "v201", "v715", "v714")
controls_men <- c("mv035", "mv133", "mv212")



# Calculate unweighted counts by group
group_counts <- data_2019 %>%
  filter(!is.na(combined_group)) %>%
  count(combined_group) %>%
  arrange(combined_group)

data_men <- data_2019 %>% filter(!is.na(mv021))

mysurvey_men <- svydesign(
  id = ~mv021, 
  strata = ~mv022,
  weights = ~wt_men,
  data = data_men,
  nest = TRUE
)

mysurvey_women <- svydesign(
  id = ~v021,
  strata = ~v022, 
  weights = ~wt_women,
  data = data_2019,
  nest = TRUE
)

options(survey.lonely.psu = "adjust") ## to account for the single-PSU Strata issue 

# Create the table
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

## Testing Parallel Trends Assumption ##
##graphs? visual representation? 
## DiD ## 
## Decision Making models ##
# the Fixed Effects Difference-in-Differences model, authors use district fixed effects, absorbs the constant? 
# using the 'svyglm' function for survey-weighted regression, 
# svyglm() models the probability that the outcome is at the non-reference level, if the outcome is a factor, 
# or the probability that the outcome is 1, if the outcome is numeric with values 0 and 1
# Define outcomes
outcomes_dec <- c("v739", "v743a", "v743b", "v743d", "v743f") 

# Run models
model_results_1 <- list()
for (i in outcomes_dec) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status * treated_district"))  # Simplified formula
  
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)  # Removed redundant data argument
  }, 
  error = function(e) {
    message(paste("Error fitting model for", i, ":", e$message)) 
    return(NULL)
  }) 
  
  if (!is.null(model_i)) {
    model_results_1[[i]] <- model_i
  }
}

# Define custom labels
custom_labels <- c(
  "Own Earnings",
  "Own Healthcare", 
  "Large Purchases",
  "Visiting Family",      
  "Husband's Money"  
)

# Create regression tables and merge
tbl_list <- lapply(model_results_1, function(model) {  # Simplified - iterate directly over models
  tbl_regression(
    model,
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_labels
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()


## DiD, with controls ##

# Run models
model_results_2 <- list()
for (i in outcomes_dec) {
  
  controls_all <- c(controls_women, controls_hh)
  controls_formula <- paste(controls_women, collapse = " + ")
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))   
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women)  
  }, 
  error = function(e) {
    message(paste("Error fitting model for", i, ":", e$message)) 
    return(NULL)
  }) 
  
  if (!is.null(model_i)) {
    model_results_2[[i]] <- model_i
  }
}


# Create regression tables and merge
tbl_list <- lapply(model_results_2, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_labels
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

##IPV and Attitudes towards IPV models ##
outcomes_ipv <- c("d106", "d107", "d108", "d129")
model_results_3 <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  model_i <- tryCatch({
    svyglm(formula_i, design= mysurvey_women)
  }, 
  error= function(e){
    message(paste("Error fitting model for", i, ":", e$message)) 
    return(NULL)
  })
  
  if (!is.null(model_i)) {
    model_results_3[[i]] <- model_i
  }
}
custom_2 <- c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")

tbl_list <- lapply(model_results_3, function(model) {  
  tbl_regression(
    model,
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_2
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()


##Regressions with controls ##
outcomes_ipv <- c("d106", "d107", "d108", "d129")
model_results_4 <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  model_i <- tryCatch({
    svyglm(formula_i, design= mysurvey_women)
    }, 
  error= function(e){
    message(paste("Something got messed up somehow", i, ":", e$message)) 
    return(NULL)
  })
  
  if (!is.null(model_i)) {
    model_results_4[[i]] <- model_i
  }
}

tbl_list <- lapply(model_results_4, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_2
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

## Men's attitudes towards dec, violence, sex ## 
outcomes_men_1 <-c("mv633a", "mv633b", "mv633d", "mv634a", "mv634b", "mv634c", "mv634d") ##wife can refuse sex
outcomes_men_2 <-c("mv739", "mv743a", "mv743b") #person who decides on certain things
outcomes_men_3 <-c("mv744a", "mv744b", "mv744c", "mv744d", "mv744e") #violence justified 
custom_men_1 <-c("Refuse in case husband has sti", "Refuse in case husband has other women", "Refuse in case wife is tired", 
                 "Refuse: right to get angry", "Refuse: right to withhold financial means", "Refuse: right to use force", 
                 "Refuse, right to have sex with another woman")
custom_men_1 <-c("Refuse sex in case husband has sti", "Refuse sex in case husband has other women", "Refuse sex in case wife is tired", 
                 "Refuse sex: right to get angry", "Refuse sex: right to withhold financial means", "Refuse sex: right to use force", 
                 "Refuse sex, right to have sex with another woman")
custom_men_2 <-c("Own Earnings", "Healthcare", "Large Purchases")
custom_men_3 <-c("Justified: wife goes out without telling", "Justified: wife neglects the children", "Justified: wife argues with husband", "Justified: wife refuses to have sex", "Justified: wife burns food")

##Refusing sex Models without controls##
model_results_5 <- list()

for (i in outcomes_men_1) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_5[[i]] <- model_i
  }
} 
tbl_list <- lapply(model_results_5, function(model) {  
  tbl_regression(
    model,
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_1
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()


##Decision making models without controls ##
model_results_6 <- list()

for (i in outcomes_men_2) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_6[[i]] <- model_i
  }
} 


tbl_list <- lapply(model_results_6, function(model) {  
  tbl_regression(
    model,
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_2
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

## Violence models, without controls
model_results_7 <- list()

for (i in outcomes_men_3) {
  
  controls_formula <- paste(controls_women, collapse = " + ")
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_7[[i]] <- model_i
  }
} 


## summarizing output 
tbl_list <- lapply(model_results_7, function(model) {  
  tbl_regression(
    model,
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_3
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()


##Men Models with controls##

model_results_8 <- list()

for (i in outcomes_men_1) {
  controls_all_men <- c(controls_men, controls_hh)
  controls_formula <- paste0(controls_all_men, collapse = " + ")
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men)
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_8[[i]] <- model_i
  }
} 


tbl_list <- lapply(model_results_8, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_1
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

##Decision making models with controls ##
model_results_9 <- list()

for (i in outcomes_men_2) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_9[[i]] <- model_i
  }
} 


tbl_list <- lapply(model_results_9, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_2
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

## Violence models, without controls
model_results_10 <- list()

for (i in outcomes_men_3) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, design = mysurvey_men) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_10[[i]] <- model_i
  }
} 


tbl_list <- lapply(model_results_10, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_men_3
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()



##Effects on Workforce Participation of Women## 
outcomes_work <- c("v714", "v714a", "v746", "v731")
model_results_11 <- list()
custom_3 <- c("currently working", "has a job, currently absent", "earns more than husband", "worked in last 12 months")

for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status"))
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_women,
    ) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_11[[i]] <- model_i
  }
} 


tbl_list <- lapply(model_results_11, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_3
) %>%
  modify_caption("Survey-Weighted DiD Results, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

##with controls 
model_results_12 <- list()
controls_all <- c(controls_women, controls_hh)
controls_formula <- paste(controls_all, collapse = " + ")


for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))
  model_i <- tryCatch({
    svyglm(formula_i, design = mysurvey_women
    ) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_12[[i]] <- model_i
  }
} 

tbl_list <- lapply(model_results_12, function(model) {  
  tbl_regression(
    model,
    include = c("eligibility_status", "treated_district", "eligibility_status:treated_district"),
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_merge(pattern = "{estimate} ({std.error})") %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)  
})

# Combine all models into one table
tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_3
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses."
  ) %>%
  as_flex_table()

##balancing test t-test: between control and treated districts (reporting mean and SD) sample is for women who delivered in pre intervention period (prior to 2011)
# Filtering the data for the balance test 
# 1. Filter the raw data
data_balance <- data_2019 %>%
  filter(
    b2_01 < 2011, 
    b2_02 < 2011
  )

# 2. Re-create the survey design for this specific subset
balance_survey <- svydesign(
  id = ~v021,
  strata = ~v022, 
  weights = ~wt_women,
  data = data_2019,
  nest = TRUE
) 
options(survey.lonely.psu = "adjust")

# Ensure treatment_status is a factor for the 'by' argument

balance_table <- balance_survey %>%
  tbl_svysummary(
    include = c(all_of(outcomes_dec), 
                all_of(outcomes_work), 
                all_of(outcomes_ipv), 
                all_of(controls_hh), 
                all_of(controls_women)),
    by = treated_district, # Compare treatment vs control
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no", 
    test = list(all_continuous() ~ "svy.t.test",
                all_categorical() ~ "svy.wald.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 3. Balance Test: Comparison of Means (Pre-2011 Births Only)**")

balance_table %>% as_gt() ##taking forever to run ugh


balance_table <- balance_survey %>%
  tbl_svysummary(
    include = c(all_of(outcomes_dec), 
                all_of(controls_hh), 
                all_of(controls_women)),
    by = treated_district,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(
    test = list(all_continuous() ~ "svy.t.test",
                all_categorical() ~ "svy.wald.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 3. Balance Test: Comparison of Means (Pre-2011 Births Only)**") ##is also taking forever
balance_table %>% as_gt() ##taking forever to run ugh


##trying a faster version
library(tableone)

# Create the table
balance_table <- svyCreateTableOne(
  vars = c(outcomes_dec, outcomes_work, outcomes_ipv, 
           controls_hh, controls_women),
  strata = "treated_district",
  data = balance_survey,
  test = TRUE
)

# Print it
print(balance_table, smd = TRUE)  # smd = standardized mean difference

# Export to markdown-friendly format
library(knitr)
kable(print(balance_table, printToggle = FALSE))

### Causal Graph ###
library(ggdag)
library(dagitty)

library(ggdag)

# Define DAG for your CCT study
cct_dag <- dagify(
  # Main causal pathways
  empowerment ~ cct + income + health + education + norms,
  ipv ~ cct + empowerment + income + norms + poverty,
  
  # Mediating pathways (how CCT works)
  income ~ cct,
  health ~ cct,
  education ~ cct,
  
  # What determines CCT rollout (confounders)
  cct ~ poverty + norms + infrastructure + politics,
  
  # Other relationships
  empowerment ~ health + education,
  
  # Define treatment and outcomes
  exposure = "cct",
  outcome = c("empowerment", "ipv"),
  
  # Labels for readability
  labels = c(
    cct = "CCT\nProgram",
    empowerment = "Women's\nEmpowerment",
    ipv = "Intimate\nPartner\nViolence",
    income = "Household\nIncome",
    health = "Health\nServices\nUse",
    education = "Children's\nEducation",
    poverty = "District\nPoverty",
    norms = "Gender\nNorms",
    infrastructure = "Infrastructure",
    politics = "Political\nPriorities"
  ),
  
  # Coordinates for layout (optional - helps arrange nodes nicely)
  coords = list(
    x = c(cct = 1, income = 2, health = 2, education = 2, 
          empowerment = 3, ipv = 3, poverty = 0, norms = 0, 
          infrastructure = 0, politics = 0),
    y = c(cct = 0, income = 1, health = 0, education = -1,
          empowerment = 0.5, ipv = -0.5, poverty = 1, norms = 0,
          infrastructure = -1, politics = -2)
  )
)

# Plot
ggdag(cct_dag, text = FALSE, use_labels = "label") +
  theme_void()

ggdag_paths