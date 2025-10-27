# =====================================================
# Simple Temperature Converter (Celsius ↔ Kelvin)
# =====================================================

# Load library
library(shiny)

# ---- USER INTERFACE ----
ui <- fluidPage(
  titlePanel("Temperature Converter"),
  
  # User inputs
  selectInput("unit", "Choose conversion:",
              choices = c("Celsius to Kelvin", "Kelvin to Celsius")),
  numericInput("value", "Enter value:", value = 0),
  
  # Show result
  textOutput("result")
)

# ---- SERVER ----
server <- function(input, output) {
  output$result <- renderText({
    # Get user input
    value <- input$value
    unit <- input$unit
    
    # Do the conversion
    if (unit == "Celsius to Kelvin") {
      result <- value + 273.15
      paste(value, "°C =", result, "K")
    } else {
      result <- value - 273.15
      paste(value, "K =", result, "°C")
    }
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
