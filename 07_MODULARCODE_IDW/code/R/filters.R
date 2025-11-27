# ============================================================
# filters.R — year + month filtering logic
# ============================================================

month_to_col <- c(
  "ene","feb","mar","abr","may","jun",
  "jul","ago","sep","oct","nov","dic"
)

filter_inamhi_by_date <- function(st_sf, year, month){
  
  col <- month_to_col[month]
  
  st_sel <- st_sf |> filter(anio == year)
  
  st_sel$valor_mes <- st_sel[[col]] |>
    as.character() |>
    str_replace(",", ".") |>
    str_trim() |>
    as.numeric()
  
  st_sel <- st_sel |> filter(!is.na(valor_mes))
  
  if (nrow(st_sel) < 3)
    stop("ERROR: necesitas al menos 3 estaciones válidas.")
  
  return(st_sel)
}
