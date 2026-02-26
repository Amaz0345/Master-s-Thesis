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
needed_vars <- readLines("req_var.txt") ##add the new variables, including man currently working!!
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
##need to update the list of variables to add height/weight/whatever else I added 
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
setwd("C:/Users/ananyama/Documents/Uni/Thesis/Data and Scripts/Data")

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

## excluding MP = 23, J&K =1, MH = 27, Odisha = 21, TN = 33, Naga = 13, Andaman= 35, Lakshadweep = 31, Daman & Diu = 25, Puducherry = 34, Ladakh = 37, Puducherry = 34, Chandigarh = 4 
## restricting sample to control and treated districts only 
districts_to_keep <- c(
  214,  # saharsa
  367,  # simdega
  273,  # tamenglong
  220,  # vaishali
  169,  # mahoba
  930,  # sultanpur
  871,  # east garo hills
  122,  # bhilwara
  130,  # udaipur
  291,  # dhalai
  825,  # bastar
  302,  # goalpara
  287,  # lawngtlai
  328,  # jalpaiguri
  339,  # bankura
  248,  # papum pare
  60,   # dehradun
  357,  # purbi singhbhum
  242,  # west sikkim
  842,  # north west delhi
  412,  # dhamtari
  69,   # panchkula
  470,  # patan
  36,   # kapurthala
  321,  # kamrup
  847,  # west delhi
  488,  # bharuch
  900,  # nalgonda
  562,  # dharwad
  49,   # amritsar
  581,  # kolar
  546,  # west godavari
  28,   # hamirpur
  593,  # palakkad
  585,  # north goa
  212,  # katihar
  351,  # godda
  219,  # saran
  216,  # muzaffarnagar
  191,  # azamgarh
  876,  # west garo hills
  125,  # banswara
  279,  # ukhrul
  827,  # bilaspur
  120,  # tonk
  308,  # dhemaji
  364,  # ranchi
  916,  # north tripura
  253,  # changlang
  57,   # chamoli
  340,  # puruliya
  829,  # durg
  281,  # mamit
  331,  # dakshin dinajpur
  844,  # south delhi
  243,  # south sikkim
  85,   # rewari
  40,   # fatehgarh sahib
  491,  # valsad
  310,  # dibrugarh
  857,  # kheda
  838,  # east delhi
  551,  # ysr
  567,  # davanagere
  30,   # bilaspur
  44,   # muktsar
  905,  # rangareddy
  571,  # tumkur
  591,  # kozhikode
  586   # south goa
)


##Construction of treated district versus untreated district ##
couples_2019 <- couples_2019 %>%
  filter(as.numeric(shdist) %in% districts_to_keep) %>%
  mutate(
    shdist_num = as.numeric(shdist),
    treated_district = ifelse(
      shdist_num %in% c(214, 367, 273, 220, 169, 930, 871, 122, 130, 291, 
                        825, 302, 287, 328, 339, 248, 60, 357, 242, 842, 
                        412, 69, 470, 36, 321, 847, 488, 900, 562, 49, 
                        581, 546, 28, 593, 585), 1, 0
    )
  )

##now want to work with this dataframe, to display some characteristics and to display the summary stats 

##left join for the man currently working variable 

data_2019 <- data_2019 %>%
  left_join(IAMR7EFL %>% select(mv001, mv002, mv003, mv714), 
            by = c("husband_cluster" = "mv001", 
                   "husband_hh" = "mv002", 
                   "husband_line" = "mv003"))
data_2019 <- data_2019 %>%
  left_join(IAMR7EFL %>% select(mv001, mv002, mv003, mv190a), 
            by = c("husband_cluster" = "mv001", 
                   "husband_hh" = "mv002", 
                   "husband_line" = "mv003"))
write_dta(data_2019, here("data", "2019_final.dta"))

##i also dropped women who are unmarried and below 19 years of age i think, but the code for that is missing lol

