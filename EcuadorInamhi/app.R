# app.R

# ============================================================
# LIBRARIES
# ============================================================
library(shiny)
library(sf)
library(terra)
library(leaflet)
library(viridisLite)
library(readr)
library(dplyr)
library(gstat)
library(stringr)
library(tidyr)
library(plotly)
library(zip)

# ============================================================
# 1. CLEANING FUNCTIONS
# ============================================================

clean_utf8 <- function(x){
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x
}

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

# ============================================================
# 2. ECUADOR GEOMETRY (LOCAL GEOJSON)
# ============================================================

possible_paths <- c(
  "ecuador.geojson",
  "./ecuador.geojson",
  file.path(getwd(), "ecuador.geojson")
)

file_to_use <- possible_paths[file.exists(possible_paths)][1]

if (is.na(file_to_use)) stop("ecuador.geojson not found in app directory.")

ecuador_sf <- st_read(file_to_use, quiet = TRUE)

if (is.na(st_crs(ecuador_sf)$epsg) || st_crs(ecuador_sf)$epsg != 4326) {
  ecuador_sf <- st_transform(ecuador_sf, 4326)
}

ecuador_vect <- vect(ecuador_sf)

# ============================================================
# 3. LOAD DATA (WITH COORDINATE VALIDATION)
# ============================================================

read_inamhi_file <- function(csv_path){
  
  st <- read_csv2(csv_path, show_col_types = FALSE)
  
  month_cols <- c("ene","feb","mar","abr","may","jun",
                  "jul","ago","sep","oct","nov","dic")
  
  st$latitud2  <- clean_num(st$latitud2)
  st$longitud2 <- clean_num(st$longitud2)
  st[month_cols] <- lapply(st[month_cols], clean_num)
  
  st <- st |> filter(!is.na(latitud2), !is.na(longitud2))
  st$NombreEstacion <- clean_utf8(st$NombreEstacion)
  
  st_sf <- st_as_sf(st, coords = c("longitud2", "latitud2"), crs = 4326)
  inside <- st_within(st_sf, ecuador_sf, sparse = FALSE)
  st_sf <- st_sf[inside, ]
  
  st <- st_sf |>
    mutate(
      longitud2 = st_coordinates(st_sf)[,1],
      latitud2  = st_coordinates(st_sf)[,2]
    ) |> st_drop_geometry()
  
  if (grepl("precipitacion", csv_path, ignore.case = TRUE)) {
    st$media_anual <- rowSums(st[, month_cols], na.rm = TRUE)
  } else {
    st$media_anual <- rowMeans(st[, month_cols], na.rm = TRUE)
  }
  
  st |> filter(!is.na(media_anual))
}

# ============================================================
# 4. GENERATE ISOLINES (IDW) WITHOUT LABELS
# ============================================================

generar_isolineas <- function(data, resol = 0.03){
  
  if (nrow(data) < 3)
    stop("Se requieren al menos 3 estaciones para interpolar.")
  
  pts <- data.frame(
    X = data$longitud2,
    Y = data$latitud2,
    valor = data$media_anual
  )
  
  sp::coordinates(pts) <- ~ X + Y
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  g_idw <- gstat(
    formula = valor ~ 1,
    data    = pts,
    nmax    = 12,
    set     = list(idp = 2)
  )
  
  bb <- st_bbox(ecuador_sf)
  
  grid <- expand.grid(
    x = seq(bb$xmin, bb$xmax, by = resol),
    y = seq(bb$ymin, bb$ymax, by = resol)
  )
  
  sp::coordinates(grid) <- ~ x + y
  sp::gridded(grid)     <- TRUE
  sp::proj4string(grid) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  pred <- predict(g_idw, grid)
  
  r_idw <- rast(pred, type = "xyz", crs = "EPSG:4326")
  r_ecu <- mask(r_idw, ecuador_vect)
  
  cont <- as.contour(r_ecu)
  cont_sf <- st_as_sf(cont)
  
  list(rast = r_ecu, cont = cont_sf)
}

# ============================================================
# 5. UI
# ============================================================

ui <- fluidPage(
  
  tags$h2("Interpolación Climática – Ecuador",
          style="text-align:center; font-weight:bold; margin-bottom:20px;"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "variable", "Seleccione la variable:",
        choices = c(
          "Temperatura Media Mensual",
          "Precipitación Acumulada Anual"
        ),
        selected = "Temperatura Media Mensual"
      ),
      
      selectInput("year", "Seleccione un año:", choices = NULL),
      
      # ======================================================
      # NUEVO INPUT: RESOLUCIÓN DEL GRID (1 A 5 KM)
      # ======================================================
      selectInput(
        "grid_res",
        "Resolución del grid (km):",
        choices = c(
                    "2 km" = 0.02,
                    "3 km" = 0.03,
                    "4 km" = 0.04,
                    "5 km" = 0.05,
                    "10 km" = 0.10),
        selected = 0.05
      ),
      tags$p(
        style="color:#aa0000; font-size:12px;",
        "Nota: una malla de resolución menor (2–4 km) puede incrementar significativamente el tiempo de generación."
      ),
      
      uiOutput("station_selector"),
      
      hr(),
      h4("Descargas"),
      downloadButton("download_station_csv", "Estación seleccionada (CSV)"),
      br(), br(),
      downloadButton("download_csv", "Datos del año (CSV)"),
      downloadButton("download_shp", "Shapefiles (Isolíneas + Estaciones)"),
      
      hr(),
      h4("Fuente de Datos"),
      tags$p(
        "Catálogo de Datos Abiertos INAMHI Ecuador ",
        tags$a("https://www.datosabiertos.gob.ec/dataset/?q=inamhi",
               href="https://www.datosabiertos.gob.ec/dataset/?q=inamhi",
               target="_blank")
      ),
      
      hr(),
      h4("Metodología de Interpolación"),
      tags$p("Método: Inverse Distance Weighting (IDW)."),
      tags$p("Parámetros: potencia idp = 2; máximo estaciones = 12."),

      hr(),
      h4("Créditos"),
      tags$p(style="font-size:12px;",
        "Desarrollado por Dr. Cesar Ivan Alvarez – University of Augsburg"
      ),
      tags$a(style="font-size:13px;","cesar.alvarez@uni-a.de", href="mailto:cesar.alvarez@uni-a.de")
    ),
    
    mainPanel(
      div(
        style="display:flex; flex-direction:column; gap:10px; height:90vh;",
        leafletOutput("map", height="45vh"),
        plotlyOutput("monthly_plot", height="45vh")
      )
    )
  )
)

# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session){
  
  full_data <- reactive({
    if (input$variable == "Temperatura Media Mensual") {
      read_inamhi_file("inamhi-temperaturamedia-2019diciembre.csv")
    } else {
      read_inamhi_file("inamhi-precipitacion-2019diciembre.csv")
    }
  })
  
  observe({
    current_year <- isolate(input$year)
    years_available <- sort(unique(full_data()$anio))
    
    if (!is.null(current_year) && current_year %in% years_available) {
      updateSelectInput(session, "year",
                        choices = years_available,
                        selected = current_year)
    } else {
      updateSelectInput(session, "year",
                        choices = years_available,
                        selected = years_available[1])
    }
  })
  
  rv <- reactiveValues(selected_station = NULL)
  
  data_year <- reactive({
    req(input$year)
    full_data() |> filter(anio == input$year)
  })
  
  output$station_selector <- renderUI({
    selectInput("station", "Seleccione la estación:",
                choices = unique(data_year()$NombreEstacion),
                selected = rv$selected_station)
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    selected <- data_year() |>
      filter(longitud2 == click$lng & latitud2 == click$lat) |>
      pull(NombreEstacion)
    
    if (length(selected) > 0) {
      rv$selected_station <- selected[1]
      updateSelectInput(session, "station", selected = rv$selected_station)
    }
  })
  
  iso_data <- reactive({
    dy <- data_year()
    withProgress(message = "Generando mapa...", value = 0, {
      incProgress(0.3)
      iso <- generar_isolineas(dy, resol = as.numeric(input$grid_res))
      incProgress(0.7)
      iso
    })
  })
  
  # ============================================================
  # MAP OUTPUT
  # ============================================================
  
  output$map <- renderLeaflet({
    
    iso <- iso_data()
    req(iso)
    
    estaciones   <- data_year()
    seleccionada <- rv$selected_station %||% input$station
    
    estaciones$color <- ifelse(
      estaciones$NombreEstacion == seleccionada,
      "yellow", "red"
    )
    
    r_ecu  <- iso$rast
    cont   <- iso$cont
    
    pal_fun <- if (input$variable == "Temperatura Media Mensual") magma else viridis
    
    pal <- colorNumeric(
      pal_fun(256),
      values(r_ecu),
      na.color="transparent"
    )
    
    legend_title <- if (input$variable == "Temperatura Media Mensual") {
      "Temperatura media anual (°C)"
    } else {
      "Precipitación anual acumulada (mm)"
    }
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addRasterImage(r_ecu, colors = pal, opacity = 0.7, project = FALSE) %>%
      
      addLegend(pal=pal, values=values(r_ecu), title=legend_title) %>%
      
      addPolylines(data=cont, color="black", weight=2) %>%
      
      addCircleMarkers(
        lng=estaciones$longitud2,
        lat=estaciones$latitud2,
        radius=7,
        color=estaciones$color,
        stroke=TRUE,
        fillOpacity=1,
        layerId=estaciones$NombreEstacion,
        popup=estaciones$NombreEstacion
      )
  })
  
  
  # ============================================================
  # MONTHLY PLOT
  # ============================================================
  
  output$monthly_plot <- renderPlotly({
    
    req(input$station)
    
    df <- full_data() |> filter(NombreEstacion == input$station)
    
    df_long <- df |>
      pivot_longer(
        cols = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"),
        names_to = "mes",
        values_to = "valor"
      ) |>
      mutate(
        mes_num = match(mes, c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),
        fecha   = as.Date(paste(anio, mes_num, "15", sep="-"))
      ) |>
      arrange(fecha)
    
    plot_title <- if (input$variable == "Temperatura Media Mensual") {
      paste("Serie histórica de temperatura –", input$station)
    } else {
      paste("Serie histórica de precipitación –", input$station)
    }
    
    y_title <- if (input$variable == "Temperatura Media Mensual") "°C" else "mm"
    
    plot_ly(
      data  = df_long,
      x     = ~fecha,
      y     = ~valor,
      type  = "scatter",
      mode  = "lines+markers",
      line  = list(color="black"),
      marker= list(color="orange", size=6)
    ) %>%
      layout(
        title = plot_title,
        xaxis = list(title = "Fecha"),
        yaxis = list(title = y_title)
      )
  })
  
  # ============================================================
  # DOWNLOAD STATION CSV
  # ============================================================
  
  output$download_station_csv <- downloadHandler(
    filename = function(){
      est <- input$station
      var <- if (input$variable == "Temperatura Media Mensual") "temperatura" else "precipitacion"
      paste0("estacion_", est, "_", var, "_", input$year, ".csv")
    },
    content = function(file){
      df_est <- full_data() |> filter(NombreEstacion == input$station)
      write_csv(df_est, file)
    }
  )
  
  # ============================================================
  # DOWNLOAD YEAR CSV
  # ============================================================
  
  output$download_csv <- downloadHandler(
    filename = function(){
      var <- if (input$variable == "Temperatura Media Mensual") "temperatura" else "precipitacion"
      paste0("datos_", var, "_", input$year, ".csv")
    },
    content = function(file){
      write_csv(data_year(), file)
    }
  )
  
  # ============================================================
  # DOWNLOAD SHAPEFILES
  # ============================================================
  
  output$download_shp <- downloadHandler(
    filename = function(){
      var <- if (input$variable == "Temperatura Media Mensual") "temperatura" else "precipitacion"
      paste0("ecuador_isolineas_", var, "_", input$year, ".zip")
    },
    content = function(file){
      
      iso <- iso_data()
      
      estaciones_sf <- data_year() |>
        st_as_sf(coords=c("longitud2","latitud2"), crs=4326)
      
      tmpdir <- tempdir()
      
      st_write(iso$cont, file.path(tmpdir, "isolineas.shp"),
               delete_layer=TRUE, quiet=TRUE)
      
      st_write(estaciones_sf, file.path(tmpdir, "estaciones.shp"),
               delete_layer=TRUE, quiet=TRUE)
      
      zip::zipr(
        zipfile=file,
        files=list.files(tmpdir, pattern="isolineas|estaciones",
                         full.names=TRUE)
      )
    }
  )
}

# ============================================================
# LAUNCH APP
# ============================================================

shinyApp(ui, server)
