#########################################
# Reset environment
rm(list = ls())

# Set the working directory
library(sf)

temp <- tempfile(fileext = ".zip")
download.file("https://github.com/abhatia08/india_shp_2020/archive/master.zip", 
              temp, mode = "wb")
unzip(temp, exdir = tempdir())
# Then read the shapefile
india_shp <- st_read(file.path(tempdir, "india_shp_2020-main", "district"))
# Install (if not installed) + Load required packages using the pacman package
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(sf, ggplot2)

#########################################
# MERGING SHAPEFILES - DISTRICT
base_path <- file.path(tempdir(), "india_shp_2020-master", "district")

in_district <- list.files(file.path(base_path, "states"), 
                          pattern = "*.shp", 
                          full.names = TRUE)

# Read and merge all shapefiles
in_district <- do.call(rbind, lapply(in_district, read_sf))

# Write to a new shapefile
sf::st_write(in_district,
             file.path(base_path, "in_district.shp"),
             delete_dsn = TRUE)
#########################################
# CHECK IF SHAPEFILE IS MERGED CORRECTLY

ggplot(data = in_district) +
  aes(fill = stname) +
  geom_sf() +
  theme_minimal() +
  theme(legend.position = 'none')
