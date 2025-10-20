# ==========================================================
# SHINY APP: DWD Hourly Temperature Visualizer (Interactive Plotly + Map)
# Responsive Layout + Centered Title + Dynamic Map Placement
# ==========================================================

packages <- c("shiny", "dplyr", "ggplot2", "lubridate", "stringr",
              "rvest", "curl", "plotly", "leaflet", "htmltools")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2.title {
        text-align: center;
        color: #2c3e50;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #f9f9f9;
        border-radius: 10px;
        padding: 15px;
      }
      .footer {
        font-size: 13px;
        color: #666;
        text-align: center;
        margin-top: 20px;
      }
      .map-container {
        height: 500px;
        width: 100%;
      }
    "))
  ),
  
  # Centered title
  h2(class = "title", "🌡️ DWD Hourly Temperature Visualizer (Interactive Plotly + Map)"),
  
  fluidRow(
    column(
      width = 3,
      div(class = "sidebar",
          uiOutput("station_ui"),
          uiOutput("date_selector"),
          actionButton("generate", "Generate Graphs", class = "btn-primary"),
          br(), br(),
          downloadButton("download_csv", "⬇️ Download CSV"),
          hr(),
          textOutput("station_info"),
          div(class = "footer",
              "Developed by Dr. César Iván Alvarez – University of Augsburg")
      )
    ),
    
    column(
      width = 9,
      tabsetPanel(
        tabPanel("Hourly",  plotlyOutput("hourly_plot",  height = "350px")),
        tabPanel("Daily",   plotlyOutput("daily_plot",   height = "350px")),
        tabPanel("Monthly", plotlyOutput("monthly_plot", height = "350px"))
      ),
      br(),
      h4("🗺️ Station Map"),
      div(class = "map-container",
          leafletOutput("station_map", height = "500px"))
    )
  )
)

# ----------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(stations_df = NULL, available_df = NULL,
                       data = NULL, daily = NULL, monthly = NULL, station = NULL)
  
  # === 1. Load available stations ===
  observe({
    output$station_ui <- renderUI({ h5("⏳ Loading stations...") })
    
    stations_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/TU_Stundenwerte_Beschreibung_Stationen.txt"
    tmp_stations <- tempfile(fileext = ".txt")
    download.file(stations_url, tmp_stations, mode = "wb")
    
    stations <- read.table(tmp_stations, header = TRUE, sep = "", fileEncoding = "latin1",
                           stringsAsFactors = FALSE, fill = TRUE, strip.white = TRUE) %>%
      mutate(Stations_id = sprintf("%05d", as.numeric(Stations_id)),
             geoBreite = as.numeric(geoBreite),
             geoLaenge = as.numeric(geoLaenge))
    
    # --- Scrape available ZIPs ---
    base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
    page <- tryCatch(read_html(base_url), error = function(e) NULL)
    zip_files <- page %>% html_elements("a") %>% html_attr("href") %>%
      grep("^stundenwerte_TU_\\d{5}_\\d{8}_\\d{8}_hist\\.zip$", ., value = TRUE)
    
    zip_info <- data.frame(
      file = zip_files,
      Stations_id = str_extract(zip_files, "\\d{5}"),
      start_date  = ymd(str_extract(zip_files, "_\\d{8}_") %>% str_replace_all("_", "")),
      end_date    = ymd(str_extract(zip_files, "\\d{8}_hist") %>% str_replace("_hist", ""))
    )
    
    available_stations <- stations %>%
      inner_join(zip_info, by = "Stations_id")
    
    rv$available_df <- available_stations
    rv$stations_df <- stations
    
    choices <- paste0(available_stations$Stationsname, " (", available_stations$Stations_id, ")")
    output$station_ui <- renderUI({
      selectInput("station_select", "Select Station:", choices = choices)
    })
    
    # --- Initial map centered on Germany ---
    output$station_map <- renderLeaflet({
      leaflet(stations) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~geoLaenge, lat = ~geoBreite,
          radius = 4, color = "black", fillOpacity = 0.8,
          label = ~Stationsname, layerId = ~Stations_id
        ) %>%
        setView(lng = 10.5, lat = 51.0, zoom = 6)
    })
    
    showNotification("✅ Station list loaded successfully.", type = "message")
  })
  
  # === 2. Update when user selects from dropdown ===
  observeEvent(input$station_select, {
    req(rv$available_df)
    station_id <- str_extract(input$station_select, "\\d+")
    row <- rv$available_df %>% filter(Stations_id == station_id)
    if (nrow(row) == 0) return(NULL)
    
    rv$station <- list(
      name = row$Stationsname,
      lat = row$geoBreite,
      lon = row$geoLaenge,
      start = row$start_date,
      end = row$end_date,
      zip_file = row$file
    )
    
    output$date_selector <- renderUI({
      dateRangeInput(
        "daterange", "Select Time Window:",
        start = row$start_date,
        end   = row$end_date,
        min   = row$start_date,
        max   = row$end_date
      )
    })
    
    output$station_info <- renderText({
      paste0("📍 Station: ", rv$station$name,
             " | Lat: ", round(rv$station$lat, 4),
             " | Lon: ", round(rv$station$lon, 4),
             " | Data: ", rv$station$start, " → ", rv$station$end)
    })
    
    # --- Highlight station on map ---
    leafletProxy("station_map", data = rv$stations_df) %>%
      clearMarkers() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~geoLaenge, lat = ~geoBreite,
        radius = 4, color = "black", fillOpacity = 0.8,
        label = ~Stationsname, layerId = ~Stations_id
      ) %>%
      addCircleMarkers(
        lng = rv$station$lon, lat = rv$station$lat,
        radius = 8, color = "red", fillOpacity = 0.9,
        label = paste0(rv$station$name, " (Selected)")
      ) %>%
      setView(lng = rv$station$lon, lat = rv$station$lat, zoom = 7)
  })
  
  # === 3. Allow map click to select station ===
  observeEvent(input$station_map_marker_click, {
    click <- input$station_map_marker_click
    if (is.null(click$id)) return(NULL)
    station_id <- click$id
    row <- rv$available_df %>% filter(Stations_id == station_id)
    if (nrow(row) == 0) return(NULL)
    
    updateSelectInput(session, "station_select",
                      selected = paste0(row$Stationsname, " (", row$Stations_id, ")"))
  })
  
  # === 4. Read data from DWD ZIP ===
  observeEvent(input$generate, {
    req(rv$station)
    base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
    zip_url <- paste0(base_url, rv$station$zip_file)
    
    showNotification(paste("📡 Reading data for", rv$station$name, "..."), type = "message")
    
    tmpcon <- curl_download(zip_url, tempfile(fileext = ".zip"))
    zip_list <- unzip(tmpcon, list = TRUE)
    data_file <- zip_list$Name[grepl("produkt_tu_stunde_.*\\.txt$", zip_list$Name)]
    
    df <- read.csv2(unz(tmpcon, data_file), fileEncoding = "latin1", stringsAsFactors = FALSE) %>%
      mutate(Datetime = ymd_h(MESS_DATUM),
             Temperature_C = as.numeric(TT_TU)) %>%
      filter(!is.na(Temperature_C), Temperature_C > -100) %>%
      arrange(Datetime)
    
    rv$data <- df
    rv$daily <- df %>% group_by(Date = as.Date(Datetime)) %>%
      summarise(DailyMeanTemp = mean(Temperature_C, na.rm = TRUE))
    rv$monthly <- df %>% mutate(YearMonth = floor_date(Datetime, "month")) %>%
      group_by(YearMonth) %>%
      summarise(MonthlyMeanTemp = mean(Temperature_C, na.rm = TRUE))
    
    showNotification("✅ Data loaded from DWD.", type = "message")
  })
  
  # === 5. Plotly interactive charts ===
  observeEvent(input$generate, {
    req(rv$data, input$daterange)
    df  <- rv$data %>% filter(Datetime >= input$daterange[1], Datetime <= input$daterange[2])
    dly <- rv$daily %>% filter(Date >= input$daterange[1], Date <= input$daterange[2])
    mth <- rv$monthly %>% filter(YearMonth >= input$daterange[1], YearMonth <= input$daterange[2])
    
    # Hourly
    output$hourly_plot <- renderPlotly({
      p <- ggplot(df, aes(Datetime, Temperature_C)) +
        geom_line(color = "firebrick", linewidth = 0.6) +
        theme_minimal(14) +
        labs(title = paste("Hourly Temperature -", rv$station$name),
             x = "DateTime", y = "Temperature (°C)")
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    })
    
    # Daily
    output$daily_plot <- renderPlotly({
      p <- ggplot(dly, aes(Date, DailyMeanTemp)) +
        geom_line(color = "steelblue", linewidth = 0.8) +
        geom_smooth(span = 0.2, color = "darkred", se = FALSE) +
        theme_minimal(14) +
        labs(title = paste("Daily Mean Temperature -", rv$station$name),
             x = "Date", y = "Temperature (°C)")
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    })
    
    # Monthly
    output$monthly_plot <- renderPlotly({
      p <- ggplot(mth, aes(YearMonth, MonthlyMeanTemp)) +
        geom_line(color = "darkgreen", linewidth = 0.8) +
        geom_point(size = 1.3) +
        geom_smooth(method = "lm", se = FALSE, color = "orange") +
        theme_minimal(14) +
        labs(title = paste("Monthly Mean Temperature -", rv$station$name),
             x = "Month", y = "Temperature (°C)")
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    })
  })
  
  # === 6. Download CSV ===
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("DWD_", gsub(" ", "_", rv$station$name), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data, input$daterange)
      df <- rv$data %>%
        filter(Datetime >= input$daterange[1], Datetime <= input$daterange[2])
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ----------------------------------------------------------
shinyApp(ui, server)
