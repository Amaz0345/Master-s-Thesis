# ============================================================
#  IGMSY ANALYSIS — DATA PREPARATION
# ============================================================

# ── Libraries ────────────────────────────────────────────────
packages <- c("haven", "tidyverse", "survey", "gtsummary", "flextable", "officer",
              "broom", "here", "labelled", "tableone", "expss")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(haven)
library(tidyverse)
library(expss)
library(labelled)
library(survey)


# ============================================================
#  DIRECTORY SETUP
# ============================================================

# !! CHANGE THIS to wherever the project folder lives !!
project_root <- "C:/Users/YourName/Documents/IGMSY"   # Windows
# project_root <- "~/Documents/IGMSY"                 # Mac/Linux

# Sub-directories
script_dir <- file.path(project_root, "scripts")
data_dir   <- file.path(project_root, "data")
output_dir <- file.path(project_root, "outputs")

# Create folders if they don't exist
for (d in c(script_dir, data_dir, output_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  } else {
    message("Already exists: ", d)
  }
}

setwd(project_root)
message("Working directory set to: ", project_root)

# ── Expected data files ──────────────────────────────────────
# Data is hosted on GWDG OwnCloud. Download manually from:
# https://owncloud.gwdg.de/index.php/s/q11668N0KOUsjUK
# Save all downloaded files into:  project_root/data/
expected_files <- c(
  "IAPR7EFL.DTA",   # PR (household) data
  "IAIR7EFL.DTA",   # Individual recode (women)
  "IAMR7EFL.DTA",   # Men's recode
  "data_2019.dta"   # Pre-cleaned dataset
)

missing_files <- expected_files[!file.exists(file.path(data_dir, expected_files))]
if (length(missing_files) == 0) {
  message("All data files found in /data.")
} else {
  message("Missing files — please download from OwnCloud:")
  message(paste(" -", missing_files, collapse = "\n"))
}


# ============================================================
#  LOAD RAW DATA
# ============================================================

# Helper: read only the variables listed in a req_var.txt file
read_dhs <- function(filepath, req_var_path) {
  needed_vars <- readLines(req_var_path)
  all_vars    <- names(read_dta(filepath, n_max = 1))
  vars_to_use <- intersect(needed_vars, all_vars)
  read_dta(filepath, col_select = any_of(vars_to_use))
}

# ── Men's data ───────────────────────────────────────────────
men_2019 <- read_dhs(
  filepath      = file.path(data_dir, "IAMR7EFL.DTA"),
  req_var_path  = file.path(data_dir, "req_var_men.txt")   
)

# Preserve husband identifiers before merge
men_2019 <- men_2019 %>%
  mutate(
    husband_cluster = mv001,
    husband_hh      = mv002,
    husband_line    = mv003
  )

# ── Women's data ─────────────────────────────────────────────
women_2019 <- read_dhs(
  filepath      = file.path(data_dir, "IAIR7EFL.DTA"),
  req_var_path  = file.path(data_dir, "req_var_women.txt") 
)


# ============================================================
#  MERGE — BUILD COUPLES DATASET
# ============================================================

# Left join: match women to their husbands via cluster/household/line number
couples_2019 <- women_2019 %>%
  left_join(
    men_2019,
    by = c(
      "v001" = "mv001",  # Cluster ID
      "v002" = "mv002",  # Household ID
      "v034" = "mv003"   # Women's husband line ref → men's line number
    ),
    suffix = c("_wife", "_husband")
  )

# Verification: check for unmatched women (no husband in sample)
unmatched_rate <- mean(is.na(couples_2019$v003))
message("Unmatched women: ", round(unmatched_rate * 100, 1), "%")

# Check couple counts by state
state_summary <- couples_2019 %>%
  group_by(v024) %>%
  summarise(n_couples = n(), .groups = "drop") %>%
  arrange(v024)
print(state_summary, n = Inf)


# ============================================================
#  MERGE — ADD DISTRICT IDENTIFIERS FROM PR DATA
# ============================================================

pr_data <- read_dta(file.path(data_dir, "IAPR7EFL.DTA")) %>%
  select(hv001, hv002, hvidx, shdist)

# Add district for wife
couples_2019 <- couples_2019 %>%
  left_join(pr_data, by = c("v001" = "hv001", "v002" = "hv002", "v003" = "hvidx"))

# Add district for husband (separate join, then rename to avoid collision)
couples_2019 <- couples_2019 %>%
  left_join(
    pr_data,
    by = c("husband_cluster" = "hv001", "husband_hh" = "hv002", "husband_line" = "hvidx"),
    suffix = c("_wife", "_husband")
  ) %>%
  rename(shdist_husband = shdist_husband)   # already suffixed; rename for clarity

# Check district–state mapping
district_state_summary <- couples_2019 %>%
  filter(!is.na(shdist_wife)) %>%
  mutate(
    state_name    = as_factor(v024),
    district_name = as_factor(shdist_wife)
  ) %>%
  group_by(state_name, district_name) %>%
  summarise(n_couples = n(), .groups = "drop") %>%
  arrange(state_name, district_name)
print(district_state_summary, n = Inf)

# Sampling weight
couples_2019 <- couples_2019 %>%
  mutate(wt = v005 / 1000000)

write_dta(couples_2019, file.path(data_dir, "couples_2019.dta"))


# ============================================================
#  ADDITIONAL VARIABLES (joined from full raw files)
# ============================================================
# These variables were not in the original req_var lists

men_extra <- read_dta(file.path(data_dir, "IAMR7EFL.DTA")) %>%
  select(mv001, mv002, mv003, sm190s, mv714, mv190a)

women_extra <- read_dta(file.path(data_dir, "IAIR7EFL.DTA")) %>%
  select(v001, v002, v003, v437, v438, v511)

data_2019 <- couples_2019 %>%
  left_join(men_extra,
            by = c("husband_cluster" = "mv001",
                   "husband_hh"      = "mv002",
                   "husband_line"    = "mv003")) %>%
  left_join(women_extra,
            by = c("v001", "v002", "v003"))

write_dta(data_2019, file.path(data_dir, "data_2019.dta"))


# ============================================================
#  SAMPLE RESTRICTION
# ============================================================

# Exclude states with their own competing programs:
#   J&K (1), Chandigarh (4), Daman & Diu (25), MP (23), MH (27),
#   Odisha (21), TN (33), Nagaland (13), Andaman (35),
#   Lakshadweep (31), Puducherry (34), Ladakh (37)
# Also exclude women under 19 (done downstream in analysis script)

# Districts to retain (treated + control)
districts_to_keep <- c(
  214,  # Saharsa
  367,  # Simdega
  273,  # Tamenglong
  220,  # Vaishali
  169,  # Mahoba
  930,  # Sultanpur
  871,  # East Garo Hills
  122,  # Bhilwara
  130,  # Udaipur
  291,  # Dhalai
  825,  # Bastar
  302,  # Goalpara
  287,  # Lawngtlai
  328,  # Jalpaiguri
  339,  # Bankura
  248,  # Papum Pare
  60,  # Dehradun
  357,  # Purbi Singhbhum
  242,  # West Sikkim
  842,  # North West Delhi
  412,  # Dhamtari
  69,  # Panchkula
  470,  # Patan
  36,  # Kapurthala
  321,  # Kamrup
  847,  # West Delhi
  488,  # Bharuch
  900,  # Nalgonda
  562,  # Dharwad
  49,  # Amritsar
  581,  # Kolar
  546,  # West Godavari
  28,  # Hamirpur
  593,  # Palakkad
  585,  # North Goa
  212,  # Katihar
  351,  # Godda
  219,  # Saran
  216,  # Muzaffarnagar
  191,  # Azamgarh
  876,  # West Garo Hills
  125,  # Banswara
  279,  # Ukhrul
  827,  # Bilaspur (CG)
  120,  # Tonk
  308,  # Dhemaji
  364,  # Ranchi
  916,  # North Tripura
  253,  # Changlang
  57,  # Chamoli
  340,  # Puruliya
  829,  # Durg
  281,  # Mamit
  331,  # Dakshin Dinajpur
  844,  # South Delhi
  243,  # South Sikkim
  85,  # Rewari
  40,  # Fatehgarh Sahib
  491,  # Valsad
  310,  # Dibrugarh
  857,  # Kheda
  838,  # East Delhi
  551,  # YSR
  567,  # Davanagere
  30,  # Bilaspur (HP)
  44,  # Muktsar
  905,  # Rangareddy
  571,  # Tumkur
  591,  # Kozhikode
  586   # South Goa
)

# Treated districts (received IGMSY)
treated_districts <- c(
  214, 367, 273, 220, 169, 930, 871, 122, 130, 291,
  825, 302, 287, 328, 339, 248,  60, 357, 242, 842,
  412,  69, 470,  36, 321, 847, 488, 900, 562,  49,
  581, 546,  28, 593, 585
)

data_2019 <- data_2019 %>%
  filter(as.numeric(shdist_wife) %in% districts_to_keep) %>%
  mutate(
    shdist_num       = as.numeric(shdist_wife),
    treated_district = as.integer(shdist_num %in% treated_districts)
  )



############################################# END ###################################################
