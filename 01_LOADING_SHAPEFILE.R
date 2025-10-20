# Check if the sf package is installed, and install it only if it's missing
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")}

library(sf) # Load the sf library
shapefile_path <- "C:/uoc/CNTR_RG_20M_2024_4326.shp/CNTR_RG_20M_2024_4326.shp" 
# Define the path to your shapefile https://ec.europa.eu/eurostat/web/gisco/geodata/administrative-units/countries
shapefile_data <- st_read(shapefile_path) # Read the shapefile

plot(shapefile_data$geometry) # Plot the shapefile
crs_info <- st_crs(shapefile_data) # Get CRS (Coordinate Reference System)
print(paste("CRS:", crs_info$epsg, "-", crs_info$proj4string))
fields <- colnames(shapefile_data) # Get field names (attribute columns)
print(fields)
