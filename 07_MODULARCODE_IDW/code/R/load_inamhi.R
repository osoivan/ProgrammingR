# ============================================================
# load_inamhi.R — robust CSV loading for INAMHI
# ============================================================

read_inamhi <- function(csv_path){
  
  st <- read_csv2(csv_path, show_col_types = FALSE)
  
  clean_num <- function(x){
    x |>
      as.character() |>
      str_replace_all(",", ".") |>
      str_replace_all("−", "-") |>
      str_replace_all("–", "-") |>
      str_replace_all("[^0-9\\.-]", "") |>
      str_trim() |>
      na_if("") |>
      as.numeric()
  }
  
  st$latitud2  <- clean_num(st$latitud2)
  st$longitud2 <- clean_num(st$longitud2)
  
  st <- st |> filter(!is.na(latitud2), !is.na(longitud2))
  
  st_sf <- st_as_sf(
    st,
    coords = c("longitud2","latitud2"),
    crs = 4326
  )
  
  st_sf$NombreEstacion <- clean_utf8(st_sf$NombreEstacion)
  
  return(st_sf)
}
