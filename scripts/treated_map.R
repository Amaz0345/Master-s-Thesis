##mapping treated and control districts 
# 1. Check the district name column in your data
head(unique(data_2019$treated_district))

# 2. Check the district name column in the shapefile
head(in_district)

pr_data <- read_dta("~/Uni/Thesis/Data and Scripts/Data/IAPR7EFL.DTA") %>%
  select(hv001, hv002, hv024, hvidx, shdist) 

# Your district names
district_lookup <- pr_data %>%
  mutate(district_name = as.character(haven::as_factor(shdist))) %>%
  distinct(shdist, district_name) %>%
  arrange(shdist)

head(district_lookup, 20)

# Shapefile district names
sort(unique(in_district$dtname))[1:20]

library(stringr)
library(fuzzyjoin)
library(sf)

# Standardize both to lowercase and remove punctuation
district_lookup <- district_lookup %>%
  mutate(district_clean = tolower(str_trim(str_remove_all(district_name, "[^a-zA-Z ]"))))

shapefile_names <- data.frame(dtname = unique(in_district$dtname)) %>%
  mutate(district_clean = tolower(str_trim(str_remove_all(dtname, "[^a-zA-Z ]"))))

# Fuzzy join
matched <- stringdist_left_join(
  district_lookup, 
  shapefile_names,
  by = "district_clean",
  max_dist = 2
)

# Check unmatched
unmatched <- matched %>% filter(is.na(dtname))
nrow(unmatched)

print(unmatched %>% select(shdist, district_name), n = 21) ##none of them are from the treated/control districts 

# Create treatment lookup from your data
in_district_mapped <- in_district %>%
  mutate(district_clean = tolower(str_trim(str_remove_all(dtname, "[^a-zA-Z ]")))) %>%
  left_join(matched %>% select(shdist, district_clean.y, district_name) %>%
              rename(district_clean = district_clean.y), 
            by = "district_clean") %>%
  left_join(treatment_lookup %>% select(shdist, treated_district), 
            by = "shdist") %>%
  mutate(status = case_when(
    treated_district == 1 ~ "Treated",
    treated_district == 0 ~ "Control",
    TRUE ~ "Not in sample"
  ))

ggplot(in_district_mapped) +
  geom_sf(aes(fill = status), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "Treated" = "#c0392b",
    "Control" = "#2980b9",
    "Not in sample" = "grey85"
  )) +
  labs(
    title = "Treated vs Control Districts",
    fill = NULL,
    caption = "Red = Treated, Blue = Control, Grey = Not in sample"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
# Do all joins on plain dataframe (drop geometry first)
in_district_df <- in_district %>%
  st_drop_geometry() %>%
  mutate(district_clean = tolower(str_trim(str_remove_all(dtname, "[^a-zA-Z ]")))) %>%
  left_join(matched %>% select(shdist, district_clean.y, district_name) %>%
              rename(district_clean = district_clean.y), 
            by = "district_clean") %>%
  left_join(treatment_lookup %>% select(shdist, treated_district), 
            by = "shdist") %>%
  mutate(status = case_when(
    treated_district == 1 ~ "Treated",
    treated_district == 0 ~ "Control",
    TRUE ~ "Not in sample"
  ))

# Re-attach geometry
in_district_mapped <- in_district %>%
  select(geometry) %>%
  bind_cols(in_district_df)

# Plot
ggplot(in_district_mapped) +
  geom_sf(aes(fill = status), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "Treated" = "#c0392b",
    "Control" = "#2980b9",
    "Not in sample" = "grey85"
  )) +
  labs(
    title = "Treated vs Control Districts",
    fill = NULL,
    caption = "Red = Treated, Blue = Control, Grey = Not in sample"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Create the lookup table
treatment_lookup <- data_2019 %>%
  mutate(district_name = as.character(haven::as_factor(shdist))) %>%
  distinct(shdist, district_name, treated_district) %>%
  mutate(district_clean = tolower(str_trim(str_remove_all(district_name, "[^a-zA-Z ]"))))

# Create a simple status lookup using matched
status_lookup <- matched %>%
  select(shdist, district_clean = district_clean.y) %>%
  left_join(treatment_lookup %>% select(shdist, treated_district), by = "shdist") %>%
  mutate(status = case_when(
    treated_district == 1 ~ "Treated",
    treated_district == 0 ~ "Control",
    TRUE ~ "Not in sample"
  )) %>%
  distinct(district_clean, status)

# Add district_clean to shapefile and merge
in_district$district_clean <- tolower(str_trim(str_remove_all(in_district$dtname, "[^a-zA-Z ]")))
in_district_mapped <- merge(in_district, status_lookup, by = "district_clean", all.x = TRUE)
in_district_mapped$status[is.na(in_district_mapped$status)] <- "Not in sample"

output_dir <- "C:/Users/ananyama/Documents/Ananya Thesis/outputs" ##keep this for always??

# 2. Assign the plot to the 'my_map' object
my_map <- ggplot(in_district_mapped) +
  geom_sf(aes(fill = status), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "Treated" = "#c0392b",
    "Control" = "#2980b9",
    "Not in sample" = "grey85"
  )) +
  labs(
    title = "Treated vs Control Districts",
    fill = NULL,
    caption = "Red = Treated, Blue = Control, Grey = Not in sample"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# 3. Export using file.path() to combine your directory and filename correctly
ggsave(
  filename = file.path(output_dir, "treatment_and_control.png"), 
  plot = my_map, 
  width = 10, 
  height = 8, 
  dpi = 300
)