############ Thesis for the Masters in Development Economics ###############
############# Does the policy IGMSY have any effects on women's and men's attitudes towards women's decision-making and domestic violence? ##########
###################### Starting the actual Assignment ######################
################ Ananya Mazumder #####################

setwd("~/Thesis/Data/DHS India for Ananya/IR/2019-20")
install.packages("labelled")
install.packages("survey")

library(haven)
library(tidyverse)
library(stargazer)
library(expss)
library(labelled)
library(survey)


## setting up 2019 couples data ##
## Men's Data ## 
setwd("~/Thesis/Data/DHS India for Ananya/MR/2019-21/IAMR7EDT")
needed_vars <- readLines("req_var.txt")
varnames <- names(read_dta("IAMR7EFL.DTA", n_max = 1))
vars_to_use <- intersect(needed_vars, varnames)
men_2019 <- read_dta("IAMR7EFL.DTA", col_select = any_of(vars_to_use))
write_dta(men_2019, "men_2019.dta")

rm(varnames, vars_to_use, needed_vars)

## Women's Data ## 
setwd("~/Thesis/Data/DHS India for Ananya/IR/2019-21")
needed_vars <- readLines("req_var.txt")
varnames <- names(read_dta("IAIR7EFL.DTA", n_max = 1))
vars_to_use <- intersect(needed_vars, varnames)
women_2019 <- read_dta("IAIR7EFL.DTA", col_select = any_of(vars_to_use))
write_dta(women_2019, "women_2019.dta")

rm(varnames, vars_to_use, needed_vars)

##merging both datasets##
##before the merge, preserving the men's IDs 
men_2019 <- men_2019 %>%
  mutate(husband_cluster = mv001, husband_hh= mv002, husband_line = mv003)
couples_2019 <- women_2019 %>%
  left_join(men_2019, 
            by = c(
              "v001" = "mv001",  # Cluster ID
              "v002" = "mv002",  # Household ID
              "v034" = "mv003"   # Women's husband ref -> Men's line number
            ),
            suffix = c("_wife", "_husband"))

## Verification
# Check unmatched cases (women without husbands in sample)
unmatched_rate <- mean(is.na(couples_2019$v003))
message("Unmatched women: ", round(unmatched_rate*100, 1), "%") ##Unmatched women: 0%


write_dta(couples_2019, "couples_2019.dta") 
##now checking how many couples per state 
state_summary <- couples_2019 %>%
  group_by(v024) %>%
  summarise(n_couples = n())%>%
  arrange(v024)
print(state_summary, n= "max")



data_2019 <- read_dta('couples_2019.dta')


# Read the PR dataset and extract the district variable
setwd("~/Thesis/Data/DHS India for Ananya/PR/2019-20")

pr_data <- read_dta("IAPR7EFL.DTA") %>%
  select(hv001, hv002, hv024, hvidx, shdist) 

couples_2019 <- couples_2019 %>%
  left_join(pr_data, by = c("v001" = "hv001", "v002" = "hv002", "v003" = "hvidx")) 
#husband_cluster = mv001, husband_hh= mv002, husband_line = mv003
couples_2019 <- couples_2019 %>%
  left_join(pr_data, by = c("husband_cluster" = "hv001", "husband_hh" = "hv002", "husband_line" = "hvidx")) %>%
  rename(shdist_husband = shdist)
write_dta(couples_2019, "couples_2019.dta") 
##seeing the value labels for shdist 
haven::print_labels(couples_2019$shdist, "max.print")

state_summary_2 <- couples_2019 %>%
  group_by(v024_wife) %>%
  summarise(n_couples = n())%>%
  arrange(v024_wife)
print(state_summary, n= "max")

#now to check state and district mapping 
district_state_summary <- couples_2019 %>%
  filter(!is.na(shdist_wife)) %>%
  mutate(state_name = as_factor(v024),
         district_name = as_factor(shdist_wife)) %>%
  group_by(state_name, district_name) %>%
  summarise(n_couples = n(), .groups = 'drop') %>%
  arrange(state_name, district_name)
print(district_state_summary, n="max")
##exporting the same 
library(rtf)
rtf_doc <- RTF("district_state_map.rtf")
addParagraph(rtf_doc, "District-State Mapping Summary")
addTable(rtf_doc, district_state_summary)
done(rtf_doc)
##still incorrect. let's check again 
pr_check <- read_dta("IAPR7EFL.dta") %>%
  select(hv001, hv002, hvidx, hv024, shdist)

district_state_summary <- pr_check %>%
  filter(!is.na(shdist)) %>%
  mutate(state_name = as_factor(hv024),
         district_name = as_factor(shdist)) %>%
  group_by(state_name, district_name) %>%
  arrange(state_name, district_name)
print(district_state_summary, n="max")

##still incorrect, duplicates in hh member data?
pr_duplicates <- pr_data %>%
  group_by(hv001, hv002, hvidx) %>%
  filter(n() > 1) %>%
  arrange(hv001, hv002, hvidx)
##nope 

##creating the sampling weight variable 
couples_2019$wt <- couples_2019$v005/1000000


### looking at descriptives for women ####

data = apply_labels(
 
  # Education level (v106)
  v106 = "Education level",
  v106 = c(
    "No education" = 0,
    "Primary"      = 1,
    "Secondary"    = 2,
    "Higher"       = 3
  ),
  
  # Religion (v130)
  v130 = "Religion",
  v130 = c(
    "Hindu"      = 1,
    "Muslim"     = 2,
    "Christian"  = 3,
    "Sikh"       = 4,
    "Buddhist"   = 5,
    "Jain"      = 6,
    "Jewish" = 7,
    "No religion" = 8, 
    "Other" = 9
  ),

  # Literacy (v155)
  v155 = "Literacy",
  v155 = c(
    "Cannot read at all" = 0,
    "Able to read only parts of the sentence" = 1,
    "Able to read whole sentence" = 2,
    "No card with required language" = 3, 
    "Blind/visually impaired" = 4 
  ),
  
  # Children ever born (v213)
  v213 = "Currently pregnant",
  v213 = c(
    "no or unsure" = 0,
    "yes" = 1  
  ),
)
# Stata-like labeled frequency table
fre(data$v130)  # Religion
fre(data$v106)  # Education
fre(data$v155)
fre(data$v131) 
fre(data$v213)

#### have to drop women that are ineligible i.e. women who are under 19, and who live in certain states because of their own programs 

## states that are excluded include: Odisha, Madhya Pradesh, Tamil Nadu and Maharashtra 

##making the dataset even smaller, removing the states that have their own programs 
write_dta(couples_2019, "couples_2019.dta")

##now want to work with this dataframe, to display some characteristics and to display the summary stats 


##creating new categorical variables using mutate and ifelse (i don't have this variable, so can change this later)
couples_final_2019 <- couples_clean_final %>%
  mutate(modfp = 
           ifelse(v313 == 3, 1, 0)) %>%
  set_value_labels(modfp= c(yes = 1, no= 0)) %>%
  set_variable_labels(modfp = "Currently used any modern method")

##creating new categorical variables using case_when and mutate 
couples_final_2019 <- couples_clean_final %>%
  mutate(modfp = 
           case_when(v313 == 3 ~ 1,
                     v313 < 3~ 0 )) %>% 
  set_value_labels(modfp= c(yes = 1, no= 0)) %>%
  set_variable_labels(modfp = "Currently used any modern method")

##looking at the results: 
table(couples_final_2019$v313) 
table(couples_final_2019$modfp) 

##crosstabulation of modfp and place of residence, again, need to change modfp, its on a variable that i don't have 
svyby(~modfp, by = ~v025, design = mysurvey, FUN =svymean,
      vartype = c("se", "ci")) 
print_labels(couples_final_2019$v025)

table2 <- svyby(~modfp. by = ~v025, design = mysurvey, FUN = svymean,
                vartype = c("se", "ci")) 
table2

##Chi-Square results 
svychisq(~modfp+v025, mysurvey)

##Generalized linear models 
?svyglm

##logistic regression using svyglm
reg1 <- svyglm(modfp~ 1+ v025 + as.factor(v013), design =mysurvey,
               family =binomial(link = "logit")) ##as.factor used for categorical variables 
summary(reg1) 

##odd-ratios instead of coefficients:
ORreg1 <- exp(reg1$coefficients)
ORreg1

##looking at 2019 PR data for the district identifiers 
## it was shdist 

