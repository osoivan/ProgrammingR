# ============================================================
# idw_isoyetas.R — IDW interpolation + contour extraction
#
# This script builds:
#   1. A prediction grid over Ecuador
#   2. IDW interpolation using gstat
#   3. A clipped raster (masked to Ecuador)
#   4. Contour lines (isoyetas)
#
# ============================================================

generar_isoyetas <- function(stations_sf, resol = 0.02){
  
  # ------------------------------------------------------------
  # 1. Create a bounding box around Ecuador
  # ------------------------------------------------------------
  bb <- st_bbox(ecuador_sf)
  ext_ok <- ext(bb$xmin, bb$xmax, bb$ymin, bb$ymax)
  
  # ------------------------------------------------------------
  # 2. Create raster grid for interpolation
  # ------------------------------------------------------------
  r <- rast(ext_ok, resolution = resol, crs = "EPSG:4326")
  
  if (ncell(r) < 50)
    stop("Raster grid too small. Increase 'resol'.")
  
  # ------------------------------------------------------------
  # 3. Prepare station coordinates for gstat
  # ------------------------------------------------------------
  coords <- st_coordinates(stations_sf)
  coords <- apply(coords, 2, as.numeric)
  
  pts_df <- as.data.frame(stations_sf)
  pts_df$X <- coords[,1]
  pts_df$Y <- coords[,2]
  
  sp::coordinates(pts_df) <- ~ X + Y
  sp::proj4string(pts_df) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  # ------------------------------------------------------------
  # 4. IDW model
  # ------------------------------------------------------------
  g_idw <- gstat(
    formula = valor_mes ~ 1,
    data = pts_df,
    nmax = 12,
    set = list(idp = 2)
  )
  
  # ------------------------------------------------------------
  # 5. Create prediction grid
  # ------------------------------------------------------------
  grd <- expand.grid(
    x = seq(bb$xmin, bb$xmax, by = resol),
    y = seq(bb$ymin, bb$ymax, by = resol)
  )
  
  sp::coordinates(grd) <- ~ x + y
  sp::gridded(grd) <- TRUE
  sp::proj4string(grd) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  # ------------------------------------------------------------
  # 6. Predict IDW values on the grid
  # ------------------------------------------------------------
  pred <- predict(g_idw, grd)
  
  # ------------------------------------------------------------
  # 7. Convert prediction table → terra raster
  # ------------------------------------------------------------
  r_idw <- rast(pred, type = "xyz", crs = "EPSG:4326")
  
  # ------------------------------------------------------------
  # 8. Clip raster to Ecuador borders
  # ------------------------------------------------------------
  r_ecu <- mask(r_idw, ecuador_vect)
  
  if (all(is.na(values(r_ecu))))
    stop("Resulting raster is empty. Increase resolution.")
  
  # ------------------------------------------------------------
  # 9. Create contour lines (isoyetas)
  # ------------------------------------------------------------
  levels <- pretty(values(r_ecu), n = 10)
  isoyetas <- as.contour(r_ecu, levels = levels)
  
  # ------------------------------------------------------------
  # 10. Return final objects
  # ------------------------------------------------------------
  return(list(
    rast = r_ecu,
    cont = isoyetas
  ))
}
