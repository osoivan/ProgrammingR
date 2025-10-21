# ==========================================================
# 🌡️ DWD Hourly Temperature Visualizer (Stable Deploy Version)
# Works on ShinyApps.io without timeout or crash
# ==========================================================

options(encoding = "UTF-8")
Sys.setenv(R_MAX_NUM_DLLS = 999)

# ==========================================================
# SHINY APP: DWD Hourly Temperature Visualizer
# Ensures all dependencies are detected by renv and available
# ==========================================================

required_pkgs <- c(
  "shiny", "plotly", "leaflet", "dplyr", "ggplot2",
  "lubridate", "stringr", "rvest", "curl", "htmltools"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(rvest)
library(curl)
library(htmltools)



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
        line-height: 1.35;
      }
      .map-container {
        height: 300px;
        width: 100%;
      }
    "))
  ),
  
  h2(class = "title", "🌡️ DWD Hourly Temperature Visualizer"),
  
  fluidRow(
    column(
      width = 3,
      div(class = "sidebar",
          uiOutput("station_ui"),
          uiOutput("date_selector"),
          actionButton("generate", "Generate Graphs", class = "btn btn-primary"),
          br(), br(),
          downloadButton("download_csv", "⬇️ Download CSV"),
          hr(),
          textOutput("station_info"),
          div(
            class = "footer",
            HTML("
              <div>Developed by Dr. César Iván Alvarez – University of Augsburg</div>
              <div>Source: <a href='https://www.dwd.de/DE/Home/home_node.html' target='_blank'>https://www.dwd.de/DE/Home/home_node.html</a></div>
            ")
          )
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
          leafletOutput("station_map", height = "300px"))
    )
  )
)

# ----------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    stations_df = NULL, available_df = NULL,
    data = NULL, daily = NULL, monthly = NULL, station = NULL
  )
  
  # Helper: robust DWD TU zip reader (returns df or NULL)
  read_dwd_zip <- function(zip_url) {
    tmp <- tempfile(fileext = ".zip")
    tryCatch({
      curl::curl_download(zip_url, tmp)
      lst <- utils::unzip(tmp, list = TRUE)
      if (is.null(lst) || nrow(lst) == 0) return(NULL)
      f <- lst$Name[grepl("produkt_tu_stunde_.*\\.txt$", lst$Name)]
      if (length(f) == 0) return(NULL)
      
      # Use readr for reliable parsing, DWD uses semicolon and latin1
      df <- readr::read_delim(
        file = unz(tmp, f[1]),
        delim = ";",
        locale = readr::locale(encoding = "latin1"),
        show_col_types = FALSE,
        progress = FALSE,
        trim_ws = TRUE
      )
      
      if (!all(c("MESS_DATUM", "TT_TU") %in% names(df))) return(NULL)
      
      df <- df %>%
        mutate(
          Datetime = suppressWarnings(lubridate::ymd_h(MESS_DATUM)),
          Temperature_C = suppressWarnings(as.numeric(TT_TU))
        ) %>%
        filter(!is.na(Datetime), !is.na(Temperature_C), Temperature_C > -100) %>%
        arrange(Datetime)
      
      df
    }, error = function(e) NULL)
  }
  
  # === 1. Load available stations ===
  observe({
    output$station_ui <- renderUI({ h5("⏳ Loading stations...") })
    
    # Station metadata (historical catalog)
    stations_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/TU_Stundenwerte_Beschreibung_Stationen.txt"
    tmp_st <- tempfile(fileext = ".txt")
    ok <- tryCatch({ curl::curl_download(stations_url, tmp_st); TRUE }, error = function(e) FALSE)
    
    validate(need(ok, "Could not download station catalog from DWD."))
    
    stations <- tryCatch({
      read.table(
        tmp_st, header = TRUE, sep = "", fileEncoding = "latin1",
        stringsAsFactors = FALSE, fill = TRUE, strip.white = TRUE
      ) %>%
        mutate(
          Stations_id = sprintf("%05d", suppressWarnings(as.numeric(Stations_id))),
          geoBreite   = suppressWarnings(as.numeric(geoBreite)),
          geoLaenge   = suppressWarnings(as.numeric(geoLaenge))
        )
    }, error = function(e) NULL)
    
    validate(need(!is.null(stations) && nrow(stations) > 0, "Station table is empty or unreadable."))
    
    # Scrape historical ZIPs
    base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
    page <- tryCatch(read_html(base_url), error = function(e) NULL)
    
    validate(need(!is.null(page), "Could not list historical ZIP files at DWD."))
    
    hrefs <- page %>% html_elements("a") %>% html_attr("href")
    zip_files <- grep("^stundenwerte_TU_\\d{5}_\\d{8}_\\d{8}_hist\\.zip$", hrefs, value = TRUE)
    
    validate(need(length(zip_files) > 0, "No historical ZIP files found."))
    
    zip_info <- data.frame(
      file = zip_files,
      Stations_id = stringr::str_extract(zip_files, "\\d{5}"),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        start_date = lubridate::ymd(stringr::str_replace_all(stringr::str_extract(file, "_\\d{8}_"), "_", "")),
        end_date   = lubridate::ymd(stringr::str_replace(stringr::str_extract(file, "\\d{8}_hist"), "_hist", ""))
      )
    
    available_stations <- stations %>% inner_join(zip_info, by = "Stations_id")
    
    validate(need(nrow(available_stations) > 0, "No station matched between catalog and archives."))
    
    rv$stations_df  <- stations
    rv$available_df <- available_stations
    
    choices <- paste0(available_stations$Stationsname, " (", available_stations$Stations_id, ")")
    output$station_ui <- renderUI({
      selectInput("station_select", "Select Station:", choices = choices)
    })
    
    # Initial map
    output$station_map <- renderLeaflet({
      leaflet(stations) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~geoLaenge, lat = ~geoBreite,
          radius = 4, color = "black", fillOpacity = 0.8,
          label = ~Stationsname, layerId = ~Stations_id
        ) %>%
        setView(lng = 10.5, lat = 51.0, zoom = 4)
    })
    
    showNotification("✅ Station list loaded successfully.", type = "message")
  })
  
  # === 2. Update when user selects from dropdown ===
  observeEvent(input$station_select, {
    req(rv$available_df)
    station_id <- stringr::str_extract(input$station_select, "\\d+")
    row <- rv$available_df %>% filter(Stations_id == station_id)
    validate(need(nrow(row) > 0, "Selected station not found."))
    
    rv$station <- list(
      name = row$Stationsname,
      lat  = row$geoBreite,
      lon  = row$geoLaenge,
      start = row$start_date,
      end   = row$end_date,
      zip_file = row$file
    )
    
    output$date_selector <- renderUI({
      dateRangeInput(
        "daterange", "Select Time Window:",
        start = rv$station$start,
        end   = rv$station$end,
        min   = rv$station$start,
        max   = rv$station$end
      )
    })
    
    output$station_info <- renderText({
      paste0("📍 Station: ", rv$station$name,
             " | Lat: ", round(rv$station$lat, 4),
             " | Lon: ", round(rv$station$lon, 4),
             " | Data: ", rv$station$start, " → ", rv$station$end)
    })
    
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
    req(click$id)
    row <- rv$available_df %>% filter(Stations_id == click$id)
    req(nrow(row) > 0)
    updateSelectInput(session, "station_select",
                      selected = paste0(row$Stationsname, " (", row$Stations_id, ")"))
  })
  
  # === 4. Read data from DWD ZIP (historical) ===
  observeEvent(input$generate, {
    req(rv$station)
    base_url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
    zip_url  <- paste0(base_url, rv$station$zip_file)
    
    showNotification(paste("📡 Reading data for", rv$station$name, "..."), type = "message")
    
    df <- read_dwd_zip(zip_url)
    validate(need(!is.null(df) && nrow(df) > 0, "No data rows found in archive."))
    
    rv$data <- df
    rv$daily <- df %>%
      dplyr::group_by(Date = as.Date(Datetime)) %>%
      dplyr::summarise(DailyMeanTemp = mean(Temperature_C, na.rm = TRUE), .groups = "drop")
    
    rv$monthly <- df %>%
      dplyr::mutate(YearMonth = floor_date(Datetime, "month")) %>%
      dplyr::group_by(YearMonth) %>%
      dplyr::summarise(MonthlyMeanTemp = mean(Temperature_C, na.rm = TRUE), .groups = "drop")
    
    showNotification("✅ Data loaded from DWD.", type = "message")
  })
  
  # === 5. Plotly interactive charts ===
  observeEvent(input$generate, {
    req(rv$data, input$daterange)
    
    df  <- rv$data   %>% filter(Datetime >= input$daterange[1], Datetime <= input$daterange[2])
    dly <- rv$daily  %>% filter(Date     >= input$daterange[1], Date     <= input$daterange[2])
    mth <- rv$monthly%>% filter(YearMonth>= input$daterange[1], YearMonth<= input$daterange[2])
    
    # Hourly
    output$hourly_plot <- renderPlotly({
      p <- ggplot(df, aes(Datetime, Temperature_C)) +
        geom_line(color = "firebrick", linewidth = 0.6) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Hourly Temperature -", rv$station$name),
             x = "DateTime", y = "Temperature (°C)")
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
    })
    
    # Daily (trend line non-interactive)
    output$daily_plot <- renderPlotly({
      p <- ggplot(dly, aes(Date, DailyMeanTemp)) +
        geom_line(color = "steelblue", linewidth = 0.8) +
        geom_smooth(span = 0.2, color = "darkred", se = FALSE) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Daily Mean Temperature -", rv$station$name),
             x = "Date", y = "Temperature (°C)")
      
      gp <- ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
      
      # Disable hover + legend for the last trace (smooth)
      if (length(gp$x$data) >= 1) {
        gp$x$data[[length(gp$x$data)]]$hoverinfo  <- "none"
        gp$x$data[[length(gp$x$data)]]$showlegend <- FALSE
      }
      gp
    })
    
    # Monthly (trend line non-interactive)
    output$monthly_plot <- renderPlotly({
      p <- ggplot(mth, aes(YearMonth, MonthlyMeanTemp)) +
        geom_line(color = "darkgreen", linewidth = 0.8) +
        geom_point(size = 1.3) +
        geom_smooth(method = "lm", se = FALSE, color = "orange") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Monthly Mean Temperature -", rv$station$name),
             x = "Month", y = "Temperature (°C)")
      
      gp <- ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "x unified")
      
      if (length(gp$x$data) >= 1) {
        gp$x$data[[length(gp$x$data)]]$hoverinfo  <- "none"
        gp$x$data[[length(gp$x$data)]]$showlegend <- FALSE
      }
      gp
    })
  })
  
  # === 6. Download CSV ===
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("DWD_", gsub(" ", "_", ifelse(is.null(rv$station$name), "station", rv$station$name)), "_", Sys.Date(), ".csv")
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
