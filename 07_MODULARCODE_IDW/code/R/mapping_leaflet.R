# ============================================================
# mapping_leaflet.R — Leaflet visualization
#
# This script contains the function to visualize:
#   ✔ The interpolated raster (IDW result)
#   ✔ Isoyet contour lines
#   ✔ Weather stations with popups
#
# Students should learn:
#   ✔ How Leaflet adds raster images
#   ✔ How color palettes work
#   ✔ How to overlay multiple layers
#   ✔ How spatial objects integrate with Leaflet
# ============================================================

plot_leaflet_isoyetas <- function(r_ecu, isoyetas, stations){
  
  # ------------------------------------------------------------
  # 1. Create color palette for raster image
  # ------------------------------------------------------------
  # viridis(256) → safe for colorblind and good for scientific maps
  # colorNumeric → maps numeric raster values to colors
  pal <- colorNumeric(
    viridisLite::viridis(256),
    values(r_ecu),               # numeric values from the raster
    na.color = "transparent"     # no color for NA (outside Ecuador)
  )
  
  # ------------------------------------------------------------
  # 2. Initialize leaflet map with a basemap
  # ------------------------------------------------------------
  m <- leaflet() %>%
    addProviderTiles("CartoDB.Positron")  # clean grayscale basemap
  
  # ------------------------------------------------------------
  # 3. Add raster layer (IDW interpolated temperature)
  # ------------------------------------------------------------
  # addRasterImage draws a raster as an overlay on the basemap
  m <- m %>%
    addRasterImage(
      r_ecu,
      colors = pal,
      opacity = 0.7,             # transparency so basemap is visible
      project = FALSE            # avoid reprojection since CRS is already WGS84
    )
  
  # ------------------------------------------------------------
  # 4. Add legend for temperature values
  # ------------------------------------------------------------
  m <- m %>%
    addLegend(
      pal = pal,
      values = values(r_ecu),
      title = "IDW interpolación",
      opacity = 1
    )
  
  # ------------------------------------------------------------
  # 5. Add contour lines (isoyetas)
  # ------------------------------------------------------------
  # st_as_sf converts terra's SpatVector to sf so leaflet can understand it
  m <- m %>%
    addPolylines(
      data = st_as_sf(isoyetas),
      color = "white",
      weight = 2,
      opacity = 0.8
    )
  
  # ------------------------------------------------------------
  # 6. Add station points to inspect data quality
  # ------------------------------------------------------------
  m <- m %>%
    addCircleMarkers(
      data = stations,
      lng = ~st_coordinates(stations)[,1],  # extract coordinates
      lat = ~st_coordinates(stations)[,2],
      radius = 5,
      color = "red",
      fillOpacity = 0.9,
      
      # Popup (HTML-safe thanks to clean_utf8)
      popup = ~paste0(
        "<b>", NombreEstacion, "</b><br>",
        "Valor: ", valor_mes
      )
    )
  
  # ------------------------------------------------------------
  # 7. Return the map object
  # ------------------------------------------------------------
  return(m)
}
