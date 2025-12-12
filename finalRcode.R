rm(list = ls())

#setwd("C:/Users/ananyama/Documents/Uni/Thesis/Data and Scripts/Data"
setwd("//ug-uyst-ba-cifs.student.uni-goettingen.de/home/users/ananya.mazumder/Eigene Dateien/Thesis/Data/DHS India for Ananya")
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

data_2019 <- read_stata("2019_final.dta")
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


##creating the sampling weight variable for women 
data_2019$wt_women <- data_2019$v005/1000000

#creating the sampling weight variable for men
data_2019$wt_men <- data_2019$mv005/1000000
##setting survey design for women: weight = v005, PSU= v021, strata =v022)  
##to identify survey design, need the 3 above variables
##creating this variable before setting variable design (will need it later)
# Add the combined grouping variable to the original data
data_2019 <- data_2019 %>%
  mutate(
    combined_group = interaction(
      haven::as_factor(eligibility_status), 
      haven::as_factor(treated_district), 
      sep = " × "
    )
  )

# For MEN only:
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

data_2019 %>%
  select(v012) %>% 
  mutate(v012_sq = v012^2)
label(data_2019$v012_sq) = "Square of Women's Age"

controls_hh <- c("v131","v136","v137","v151","v152","v119","v120", "v121", "v122", "v123", "v124", "v125", "mv190", "sm190s", "sm191s")
controls_women <- c("v012", "v012_sq", "v133","v437", "v438", "v511", "d113", "v201", "v715", "v714")
controls_men <- c("mv035", "mv133", "mv212")
## Now the descriptives tables ##
# Create survey-design-specific balance table for women 
desc_table_1 <- mysurvey_women %>%
  tbl_svysummary(
    include = all_of(controls_women),
    by = combined_group,
    statistic = list(
      all_continuous() ~ "{mean}\n({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 3,
    missing = "no"
  ) %>%
  modify_caption("**Table 1. Survey-Weighted Descriptives for Women**")

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
  modify_caption("**Table 2. Survey-Weighted Descriptives for Men**")
desc_table_2 %>% as_gt() 

desc_table_3 <- mysurvey_women %>%
  tbl_svysummary(
    include = all_of(controls_hh), 
    by =combined_group,
    statistic= list(
      all_continuous() ~"{mean}\n({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 3,
    missing = "no"
  ) %>%
  modify_caption("**Table 3. Survey-Weighted Descriptives for Household**")
desc_table_3 %>% as_gt() 

## Testing Parallel Trends Assumption ##
##graphs? visual representation? 
## DiD ## 
## Decision Making models ##
# the Fixed Effects Difference-in-Differences model, authors use district fixed effects, absorbs the constant? 
# using the 'svyglm' function for survey-weighted regression, 
# svyglm() models the probability that the outcome is at the non-reference level, if the outcome is a factor, 
# or the probability that the outcome is 1, if the outcome is numeric with values 0 and 1
outcomes_dec <- c("v739", "v743a", "v743b", "v743c", "v743d", "v743e", "v743f") 

model_results_1 <- list()

for (i in outcomes_dec) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_women,
           data = data_2019 
    ) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_1[[i]] <- model_i
  }
} 

## summarizing output 
model_vector <- do.call(c, list(model_results_1))
summaries_list <- lapply(model_results_1, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])

custom_labels <- c(
  "Healthcare",
  "Large Purchases", 
  "Family Visits",
  "Food Purchases",
  "Own Earnings"
)

stargazer(model_vector,
          type = "text",
          title = "Survey-Weighted DiD Results, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_labels,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)

## DiD, with controls ##
model_results_3 <- list()

for (i in outcomes_dec) {
  controls_all <- c(controls_women, controls_hh)
  controls_formula <- paste(controls_women, collapse = " + ")
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_women,
           data = data_2019
    ) 
    
  }, 
  error = function(e){
    message(paste("no; something is wrong somehow", i, ":", e$message)) 
    return(NULL)
  } 
  ) 
  
  if (!is.null(model_i)) {
    model_results_3[[i]] <- model_i
  }
}
stargazer(model_results_3,
          type = "text",
          title = "Survey-Weighted DiD Results, Controls",
          keep = c("eligibility_status", "treated_district", controls_all),
          se = se_list,           
          p = p_list,             
          column.labels = custom_labels,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = TRUE)
##ran with controls, just didn't print them 
##remember to drop controls for final output


##IPV and Attitudes towards IPV models ##
outcomes_ipv <- c("d106", "d107", "d108", "d129")
model_results_2 <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  model_i <- tryCatch({
    svyglm(formula_i, 
           design= mysurvey_women,
           data = data_2019)
  }, 
  error= function(e){
    # Use the specific error message for better debugging
    message(paste("Something got messed up somehow", i, ":", e$message)) 
    return(NULL)
  })
  
  if (!is.null(model_i)) {
    model_results_2[[i]] <- model_i
  }
}

model_vector_2 <- do.call(c, list(model_results_2))
# Use lapply to get the summary for each model
summaries_list <- lapply(model_results_1, summary)
print(summaries_list)
# Extract the SEs for all coefficients from all models
se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

# Extract the P-values for all coefficients from all models
p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])

# 2. Run stargazer, passing the lists of correct SEs and P-values
custom_2 <- c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")
stargazer(model_vector_2,
          type = "text",
          title = "Survey-Weighted DiD Results, No Controls",
          se = se_list,           
          p = p_list,           
          column.labels = custom_2,
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.txt",
          t.auto = FALSE,
          p.auto = FALSE)
##Regressions with controls ##
outcomes_ipv <- c("d106", "d107", "d108", "d129")
model_results_4 <- list()
for (i in outcomes_ipv) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  model_i <- tryCatch({
    svyglm(formula_i, 
           design= mysurvey_women,
           data = data_2019)
  }, 
  error= function(e){
    # Use the specific error message for better debugging
    message(paste("Something got messed up somehow", i, ":", e$message)) 
    return(NULL)
  })
  
  if (!is.null(model_i)) {
    model_results_4[[i]] <- model_i
  }
}

model_vector_4 <- do.call(c, list(model_results_2))
# Use lapply to get the summary for each model
summaries_list <- lapply(model_results_1, summary)
print(summaries_list)
# Extract the SEs for all coefficients from all models
se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

# Extract the P-values for all coefficients from all models
p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])

# 2. Run stargazer, passing the lists of correct SEs and P-values
custom_2 <- c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")
stargazer(model_vector_4,
          type = "text",
          title = "Survey-Weighted DiD Results, with Controls",
          se = se_list,           
          p = p_list,           
          column.labels = custom_2,
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.txt",
          t.auto = FALSE,
          p.auto = FALSE)
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
    
    svyglm(formula_i, 
           design = mysurvey_men,
           data = data_2019
    ) 
    
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

## summarizing output 
model_vector_5 <- do.call(c, list(model_results_5))
summaries_list <- lapply(model_results_5, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_5,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_1,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)
##Decision making models without controls ##
model_results_6 <- list()

for (i in outcomes_men_2) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_men
    ) 
    
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


## summarizing output 
model_vector_6 <- do.call(c, list(model_results_6))
summaries_list <- lapply(model_results_6, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_6,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_2,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)

## Violence models, without controls
model_results_7 <- list()

for (i in outcomes_men_3) {
  
  controls_formula <- paste(controls_women, collapse = " + ")
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status")) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_men
    ) 
    
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
model_vector_7 <- do.call(c, list(model_results_7))
summaries_list <- lapply(model_results_7, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_7,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_3,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)


##Models with controls##

model_results_8 <- list()

for (i in outcomes_men_1) {
  controls_all_men <- c(controls_men, controls_hh)
  controls_formula <- paste0(controls_all_men, collapse = " + ")
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_men
    ) 
    
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


## summarizing output 
model_vector_8 <- do.call(c, list(model_results_8))
summaries_list <- lapply(model_results_8, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_8,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_1,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)
##Decision making models without controls ##
model_results_9 <- list()

for (i in outcomes_men_2) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_men,
    ) 
    
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


## summarizing output 
model_vector_9 <- do.call(c, list(model_results_9))
summaries_list <- lapply(model_results_9, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_9,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_2,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)

## Violence models, without controls
model_results_10 <- list()

for (i in outcomes_men_3) {
  
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula)) 
  
  model_i <- tryCatch({
    
    svyglm(formula_i, 
           design = mysurvey_men
    ) 
    
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


## summarizing output 
model_vector_10 <- do.call(c, list(model_results_10))
summaries_list <- lapply(model_results_10, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_10,
          type = "text",
          title = "Survey-Weighted DiD Results for Men, Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_men_3,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)



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


## summarizing output 
model_vector_11 <- do.call(c, list(model_results_11))
summaries_list <- lapply(model_results_11, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_11,
          type = "text",
          title = "Survey-Weighted DiD Results for Women's Workforce Participation, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_3,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)

##with controls 
outcomes_work <- c("v714", "v714a", "v746", "v731")
model_results_11 <- list()
custom_3 <- c("currently working", "has a job, currently absent", "earns more than husband", "worked in last 12 months")
controls_all <- c(controls_women, controls_hh)
controls_formula <- paste(controls_all, collapse = " + ")


for (i in outcomes_work) {
  formula_i <- as.formula(paste0(i, " ~ eligibility_status + treated_district * eligibility_status + ", controls_formula))
  model_i <- tryCatch({
    svyglm(formula_i, 
           design = mysurvey_women
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


## summarizing output 
model_vector_11 <- do.call(c, list(model_results_11))
summaries_list <- lapply(model_results_11, summary)

se_list <- lapply(summaries_list, function(x) x$coefficients[, "Std. Error"])

p_list <- lapply(summaries_list, function(x) x$coefficients[, "Pr(>|t|)"])


stargazer(model_vector_11,
          type = "text",
          title = "Survey-Weighted DiD Results for Women's Workforce Participation, No Controls",
          se = se_list,           
          p = p_list,             
          column.labels = custom_3,  
          keep.stat = c("n", "rsq"), 
          out = "did_results_final.html",
          t.auto = FALSE,
          p.auto = FALSE)

##balancing test t-test: between control and treated districts (reporting mean and SD) sample is for women delivered in pre intervention period (prior to 2011)
data_2019 %>%
  filter(eligibility_status == 1)
 select(treated_district, all_of(controls_women)) %>%
   tbl_summary(by = treated_district)

















