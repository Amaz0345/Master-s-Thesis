##women's decision making model, wihout controls 
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
  
  if (!is.null(model_i)) {
    model_results_1[[i]] <- model_i
  }
}

custom_labels_1 <- c("Own Earnings", "Own Healthcare", "Large Purchases", "Visiting Family")
custom_labels_2 <- c("Husband's Money", "Autonomy Index", "Autonomy Dummy")

outcomes_1 <- c("v739", "v743a", "v743b", "v743d")
outcomes_2 <- c("v743f", "autonomy_index", "autonomy_dummy")

make_tbl <- function(outcome_name) {
  tbl_regression(
    model_results_1[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1
tbl_merge(
  tbls = lapply(outcomes_1, make_tbl),
  tab_spanner = custom_labels_1
) %>%
  modify_caption("Survey-Weighted DiD Results: Women's Decision-making (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2
tbl_merge(
  tbls = lapply(outcomes_2, make_tbl),
  tab_spanner = custom_labels_2
) %>%
  modify_caption("Survey-Weighted DiD Results: Women's Decision-making (2/2)") %>%
  modify_footnote(
    everything() ~ "Autonomy Index measures the proportion of affirmative responses across all five decision-making domains. Autonomy Dummy equals 1 if woman has say in any household decision. Both composite measures constructed only for women who answered all 5 decision-making questions."
  ) %>%
  as_flex_table()

#with controls 


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

make_tbl <- function(outcome_name) {
  tbl_regression(
    model_results_2[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1
tbl_merge(
  tbls = lapply(outcomes_1, make_tbl),
  tab_spanner = custom_labels_1
) %>%
  modify_caption("Survey-Weighted DiD Results: Women's Decision-making (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2
tbl_merge(
  tbls = lapply(outcomes_2, make_tbl),
  tab_spanner = custom_labels_2
) %>%
  modify_caption("Survey-Weighted DiD Results: Women's Decision-making (2/2)") %>%
  modify_footnote(
    everything() ~ "Autonomy Index measures the proportion of affirmative responses across all five decision-making domains. Autonomy Dummy equals 1 if woman has say in any household decision. Both composite measures constructed only for women who answered all 5 decision-making questions."
  ) %>%
  as_flex_table()



#Categorical variables: 1-- respondent alone, 2 -- respondent and husband/partner, 4 -- husband/partner alone, 5 -- someone else. . missings 
#These are my main results, the effect of eligibility interacted with treated district on decision-making at various household dimensions. 

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
  modify_caption("Survey-Weighted DiD Results: Intimate Partner Violence Outcomes, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program impact. All outcomes are binary variables (1 = ever experienced, 0 = never experienced). Less Severe Violence (d106) includes: ever been pushed, shook, or had something thrown; ever been slapped; ever been punched with fist or hit by something harmful. Severe Violence (d107) includes: ever been kicked or dragged; ever been strangled or burnt; ever been threatened with knife/gun or other weapon. Sexual Violence (d108) includes: ever been physically forced into unwanted sex; ever been forced into other unwanted sexual acts; ever been physically forced to perform sexual acts respondent didn't want to. Afraid of Husband (d129): respondent is afraid of husband/partner most of the time or sometimes. Linear probability model used for binary outcomes. Survey weights applied using complex survey design. Sample restricted to ever-married women."
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

tbl_merge(
  tbls = tbl_list,
  tab_spanner = custom_2
) %>%
  modify_caption("Survey-Weighted DiD Results, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program impact. All outcomes are binary variables (1 = ever experienced, 0 = never experienced). Less Severe Violence (d106) includes: ever been pushed, shook, or had something thrown; ever been slapped; ever been punched with fist or hit by something harmful. Severe Violence (d107) includes: ever been kicked or dragged; ever been strangled or burnt; ever been threatened with knife/gun or other weapon. Sexual Violence (d108) includes: ever been physically forced into unwanted sex; ever been forced into other unwanted sexual acts; ever been physically forced to perform sexual acts respondent didn't want to. Afraid of Husband (d129): respondent is afraid of husband/partner most of the time or sometimes. Linear probability model used for binary outcomes. Survey weights applied using complex survey design. Sample restricted to ever-married women."
  ) %>%
  as_flex_table()
#Of particular interest in my thesis is the attitude of men towards decision-making and IPV as well. 

##Men's Attitudes towards violence, and IPV, decision-making

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
outcomes_men_1a <- c("mv633a", "mv633b", "mv633d", "mv634a")
outcomes_men_1b <- c("mv634b", "mv634c", "mv634d")

custom_men_1a <- c("Refuse sex in case husband has sti", "Refuse sex in case husband has other women", 
                   "Refuse sex in case wife is tired", "Refuse sex: right to get angry")
custom_men_1b <- c("Refuse sex: right to withhold financial means", "Refuse sex: right to use force", 
                   "Refuse sex: right to have sex with another woman")

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

make_tbl_men <- function(outcome_name) {
  tbl_regression(
    model_results_5[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1: first 4 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_1a, make_tbl_men),
  tab_spanner = custom_men_1a
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes Toward Women's Right to Refuse Sex (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2: last 3 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_1b, make_tbl_men),
  tab_spanner = custom_men_1b
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes Toward Women's Right to Refuse Sex (2/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design.Sample restricted to currently married men whose wives meet eligibility criteria"
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
  modify_caption("Survey-Weighted DiD Results, Men's Attitudes towards their own Decision-Making, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. 
    The interaction term 'eligibility_status × treated_district' represents the difference-in-differences estimate of program impact on husbands of eligible women.
    Linear regression used for all outcomes including the categorical questions relating to decision-making. Survey weights applied using complex survey design. 
    Sample restricted to currently married men whose wives meet eligibility criteria."
  ) %>%
  as_flex_table()

## Violence models, without controls
outcomes_men_3a <- c("mv744a", "mv744b", "mv744c")
outcomes_men_3b <- c("mv744d", "mv744e")

custom_men_3a <- c("Justified: wife goes out without telling", 
                   "Justified: wife neglects the children", 
                   "Justified: wife argues with husband")
custom_men_3b <- c("Justified: wife refuses to have sex", 
                   "Justified: wife burns food")

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

make_tbl_men3 <- function(outcome_name) {
  tbl_regression(
    model_results_7[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1: first 3 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_3a, make_tbl_men3),
  tab_spanner = custom_men_3a
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes towards Violence against Wives (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. All outcomes are binary variables (1 = no, 0 = yes). Linear probability model used for binary outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2: last 2 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_3b, make_tbl_men3),
  tab_spanner = custom_men_3b
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes towards Violence against Wives (2/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. All outcomes are binary variables (1 = no, 0 = yes). Linear probability model used for binary outcomes. Survey weights applied using complex survey design.Sample restricted to currently married men whose wives meet eligibility criteria."
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


make_tbl_men8 <- function(outcome_name) {
  tbl_regression(
    model_results_8[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1: first 4 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_1a, make_tbl_men8),
  tab_spanner = custom_men_1a
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes Toward Women's Right to Refuse Sex (with controls) (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2: last 3 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_1b, make_tbl_men8),
  tab_spanner = custom_men_1b
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes Toward Women's Right to Refuse Sex (with controls) (2/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. Linear regression used for all outcomes. Survey weights applied using complex survey design.Sample restricted to currently married men whose wives meet eligibility criteria."
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
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes towards own Decision-Making, Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. 
The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program impact on husbands of eligible women. 
All outcomes are binary variables where 1 = agrees with statement, 0 = disagrees. 
Responses coded as 'don't know' (8) treated as missing. 
    Linear probability model used for binary outcomes. 
    Survey weights applied using complex survey design. 
    Sample restricted to currently married men whose wives meet eligibility criteria."
  ) %>%
  as_flex_table()

## Violence models, with controls
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


make_tbl_men10 <- function(outcome_name) {
  tbl_regression(
    model_results_10[[outcome_name]],
    label = list(
      eligibility_status ~ "Eligibility Status",
      treated_district ~ "Treated District",
      `eligibility_status:treated_district` ~ "Eligibility × Treated"
    ),
    estimate_fun = function(x) style_number(x, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
    modify_column_hide(c(conf.low, conf.high)) %>%
    add_glance_table(include = nobs)
}

# Table 1: first 3 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_3a, make_tbl_men10),
  tab_spanner = custom_men_3a
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes towards Violence against Wives (1/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. All outcomes are binary variables (1 = no, 0 = yes). Linear probability model used for binary outcomes. Survey weights applied using complex survey design."
  ) %>%
  as_flex_table()

# Table 2: last 2 outcomes
tbl_merge(
  tbls = lapply(outcomes_men_3b, make_tbl_men10),
  tab_spanner = custom_men_3b
) %>%
  modify_caption("Survey-Weighted DiD Results: Men's Attitudes towards Violence against Wives (2/2)") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors in parentheses. The interaction term 'Eligibility × Treated' represents the DiD estimate of program impact. All outcomes are binary variables (1 = no, 0 = yes). Linear probability model used for binary outcomes. Survey weights applied using complex survey design.Sample restricted to currently married men whose wives meet eligibility criteria."
  ) %>%
  as_flex_table()

##Lastly, the effect of eligibilityxtreated district for the Economic Participation of Women is looked at. 

## "Women's Economic Participation Models"
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
  modify_caption("Survey-Weighted DiD Results: Women's Economic Participation, No Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. 
    The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program impact. 
    All outcomes are binary variables (1 = no, 0 = yes)
    Linear probability model used for binary outcomes. 
    Survey weights applied using complex survey design. 
    Sample restricted to ever-married women only." 
  ) %>%
as_flex_table()


##with controls 
controls_women_1 <- c("v012", "v012_sq", "v133","v437", "v438", "v511", "d113", "v201", "v715")
model_results_12 <- list()
controls_all <- c(controls_women_1, controls_hh)
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
  modify_caption("Survey-Weighted DiD Results: Women's Economic Participation, with Controls") %>%
  modify_footnote(
    everything() ~ "Coefficients with standard errors (clustered at district level) in parentheses. 
The interaction term 'Eligibility × Treated' represents the difference-in-differences estimate of program impact. 
All outcomes are binary variables (1 = no, 0 = yes)
Linear probability model used for binary outcomes. 
Survey weights applied using complex survey design. 
Sample restricted to ever-married women only." 
  ) %>%
as_flex_table()

