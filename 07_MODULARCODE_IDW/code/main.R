# ============================================================
#   main.R — run the entire pipeline by calling modular scripts
# ============================================================

library(sf)
library(terra)
library(leaflet)
library(viridisLite)
library(readr)
library(dplyr)
library(rnaturalearth)
library(gstat)
library(stringr)

setwd("C:/code")   # Adjust to your directory

# ---- load modular scripts ----
source("R/utils_utf8.R")
source("R/load_inamhi.R")
source("R/filters.R")
source("R/idw_isoyetas.R")
source("R/mapping_leaflet.R")

# ============================================================
#   Ecuador shapefile (loaded only once)
# ============================================================
ecuador_sf <- rnaturalearth::ne_countries(
  country = "Ecuador",
  returnclass = "sf",
  scale = "medium"
)
ecuador_vect <- vect(ecuador_sf)

# ============================================================
#   Data available on https://www.datosabiertos.gob.ec/dataset/temperatura-minima-absoluta
# ============================================================
inamhi_all <- read_inamhi("data/inamhi-temperaturamedia-2019diciembre.csv")

YEAR  <- 1985
MONTH <- 3

stations_sel <- filter_inamhi_by_date(inamhi_all, YEAR, MONTH)

iso <- generar_isoyetas(stations_sel, resol = 0.02)

plot_leaflet_isoyetas(iso$rast, iso$cont, stations_sel)
