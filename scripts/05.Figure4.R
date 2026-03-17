# ============================================================
#  Fig. 4: District Map 
#  Author: Ananya Mazumder 
#  References: von Haaren & Klonner (2020), abhatia08 (github)
# ===========================================================
# ============================================================
#  DISTRICT MAP: IGMSY Pilot Program
# Part I: Creating the shapefile for India 
# ============================================================
# ──  Setting up the environment for making the shapefile  ────────────────────────────────────────

temp <- tempfile(fileext = ".zip")
download.file("https://github.com/abhatia08/india_shp_2020/archive/master.zip", 
              temp, mode = "wb")
unzip(temp, exdir = tempdir())
# Then read the shapefile
india_shp <- st_read(file.path(tempdir(), "india_shp_2020-master", "district"))

# Requires: pacman, sf 
if (!require("pacman"))
  install.packages("pacman")
if (!require("sf"))
  install.packages("sf")
library(pacman)
library(sf)
#########################################
# Merging shapefiles through districts 
base_path <- file.path(tempdir(), "india_shp_2020-master", "district")

in_district <- list.files(file.path(base_path, "states"), 
                          pattern = "*.shp", 
                          full.names = TRUE)

in_district <- do.call(rbind, lapply(in_district, read_sf))

sf::st_write(in_district,
             file.path(base_path, "in_district.shp"),
             delete_dsn = TRUE)
#########################################
# Checking to see if shapefile was merged correctly 

ggplot(data = in_district) +
  aes(fill = stname) +
  geom_sf() +
  theme_minimal() +
  theme(legend.position = 'none') ##if it worked, you should have a nice, colorful map of india! 


#pr_data <- read_dta("~/Uni/Thesis/Data and Scripts/Data/IAPR7EFL.DTA") %>%
 # select(hv001, hv002, hv024, hvidx, shdist) ##from the raw data, would be too big to upload 
library(stringr)
library(fuzzyjoin)
# ── Background work to create treatment_lookup.rds ────────────────────────────────────────
## can run this with access to the India PR File for 2019-20 DHS but if not, run from after treatment_lookup.rds ## 

# district names
district_lookup <- pr_data %>%
  mutate(district_name = as.character(haven::as_factor(shdist))) %>%
  distinct(shdist, district_name) %>%
  arrange(shdist)

# ============================================================
#  DISTRICT MAP: IGMSY Pilot Program
#  Part II: Recalls district codes and names from the PR Data 
# ============================================================
treated_codes <- c(
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
  551,  # ysr
  562,  # dharwad
  49,   # amritsar
  581,  # kolar
  546,  # west godavari
  28,   # hamirpur
  593,  # palakkad
  585   # north goa
)

control_codes <- c(
  212,  # katihar
  351,  # godda
  216,  # muzaffarnagar
  219,  # saran
  191,  # azamgarh
  876,  # west garo hills
  125,  # banswara
  279,  # ukhrul
  827,  # bilaspur (chhattisgarh)
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
  567,  # davanagere
  30,   # bilaspur (himachal)
  44,   # muktsar
  905,  # rangareddy
  571,  # tumkur
  591,  # kozhikode
  586   # south goa
)

# verify counts
length(treated_codes)  # should be 36
length(control_codes)  # should be 34
# ── Extracting treatment status from survey data ────────
# Creates a lookup table of district ID (shdist), human-readable
# district name, and whether the district is treated or control
treatment_lookup <- pr_data %>%
  mutate(district_name = as.character(haven::as_factor(shdist))) %>%
  distinct(shdist, district_name) %>%
  mutate(
    district_clean = clean_name(district_name),
    treated_district = case_when(
      shdist %in% treated_codes ~ 1,
      shdist %in% control_codes ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(treated_district))

saveRDS(treatment_lookup, file = file.path(output_dir, "treatment_lookup.rds"))

# verify
table(treatment_lookup$treated_district)
# ============================================================
#  DISTRICT MAP: IGMSY Pilot Program
#  Part III: Maps treated and control districts onto shapefile
# ============================================================

# ── RUN FROM HERE: Standardize shapefile district names ─────────────
clean_name <- function(x) {
  tolower(str_trim(str_remove_all(x, "[^a-zA-Z ]"))) %>%
    str_replace_all("\\s+", " ") %>%  # collapse multiple spaces
    str_trim()
}

# apply to treatment_lookup
treatment_lookup <- treatment_lookup %>%
  mutate(district_clean = clean_name(district_name))

# apply to shapefile
in_district$district_clean <- clean_name(in_district$dtname)
# Optional: inspect first 20 shapefile district names to check format
#sort(unique(in_district$dtname))[1:20]

# ── Match survey districts to shapefile districts ────
# inner_join keeps only districts that appear in BOTH the survey
# data and the shapefile — i.e. districts we can actually map
matched <- treatment_lookup %>%
  inner_join(
    in_district %>% st_drop_geometry() %>% distinct(district_clean),
    by = "district_clean"
  )

# Check how many survey districts did not match the shapefile
unmatched <- treatment_lookup %>%
  filter(!district_clean %in% matched$district_clean)
message(nrow(unmatched), " unmatched districts")
print(unmatched %>% select(shdist, district_name), n = Inf) ##now only 2 unmatched 

# ── Step 4: Build status lookup ──────────────────────────────
# For each matched district, assign a status label of "treated" or "control"
status_lookup <- matched %>%
  select(shdist, district_clean) %>%
  left_join(
    treatment_lookup %>% select(shdist, treated_district),
    by = "shdist"
  ) %>%
  mutate(status = case_when(
    treated_district == 1 ~ "Treated",
    treated_district == 0 ~ "Control",
    TRUE                  ~ "Not in sample"
  )) %>%
  distinct(district_clean, status)

# ── Merge status onto shapefile ──────────────────────
# all.x = TRUE keeps ALL shapefile districts (even unmatched ones)
# Unmatched districts get NA for status, filled in as "Not in sample"
in_district_mapped <- merge(in_district, status_lookup, 
                            by = "district_clean", all.x = TRUE)
in_district_mapped$status[is.na(in_district_mapped$status)] <- "Not in sample"

# ── Plot ─────────────────────────────────────────────
my_map <- ggplot(in_district_mapped) +
  # geom_sf draws the shapefile polygons, filled by treatment status
  geom_sf(aes(fill = status), color = "white", linewidth = 0.1) +
  scale_fill_manual(values = c(
    "Treated"       = "#c0392b",  # red
    "Control"       = "#2980b9",  # blue
    "Not in sample" = "grey85"
  )) +
  labs(
    title   = "District Selection for IGMSY Pilot Program",
    fill    = NULL,
    caption = paste0(
      "Source: Districts from von Haaren and Klonner (2020); ",
      "Shapefile via abhatia08 (GitHub); ",
      "Author's own figure."
    )
  ) +
  theme_void() +
  theme(
    plot.title     = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption   = element_text(hjust = 0)
  )

# ── Save ─────────────────────────────────────────────
ggsave(
  filename = file.path(output_dir, "fig4.png"),
  plot     = my_map,
  width    = 10,
  height   = 8,
  dpi      = 300,
  bg      = "white"
)
message("Saved: fig4.png")
############################################# END ###################################################
