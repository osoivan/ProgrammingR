# =====================================================
# Simple Temperature Converter (Celsius ↔ Kelvin)
# Introduction to shiny https://mastering-shiny.org/basic-app.html
# =====================================================

###########################################################
# STEP 0 — INSTALL (only once, in the console, not in app)
###########################################################
# install.packages("shiny")   # <- Run this once if Shiny is not installed

###########################################################
# STEP 1 — LOAD THE SHINY PACKAGE
###########################################################
# We need to load the shiny library so we can use its functions
library(shiny)


###########################################################
# STEP 2 — DEFINE THE USER INTERFACE (UI)
###########################################################
# The UI controls WHAT the user sees on the screen:
# titles, input boxes, dropdowns, plots, text, etc.
#
# Here we use fluidPage(), a simple layout that places
# elements one under another from top to bottom.
ui <- fluidPage(
  
  # 2.1 — Add a title at the top of the app
  titlePanel("Temperature Converter"),
  
  # 2.2 — Add a dropdown so the user can choose the type of conversion
  # inputId = "unit" is the name we will use in the server as input$unit
  selectInput(
    inputId = "unit",
    label   = "Choose conversion:",
    choices = c("Celsius to Kelvin", "Kelvin to Celsius")
  ),
  
  # 2.3 — Add a numeric input so the user can enter a temperature value
  # inputId = "value" will be used in the server as input$value
  numericInput(
    inputId = "value",
    label   = "Enter value:",
    value   = 0      # default value
  ),
  
  # 2.4 — Add an output area where we will print the result as text
  # In the server we will use output$result
  textOutput("result")
)


###########################################################
# STEP 3 — DEFINE THE SERVER LOGIC
###########################################################
# The server controls HOW the app reacts to user inputs.
# It is a function with two arguments: input and output.
#
# - input  : values that come FROM the UI (input$unit, input$value, etc.)
# - output : places in the UI that we FILL from the server (output$result, etc.)
server <- function(input, output) {
  
  # 3.1 — Create the text that will be shown in textOutput("result")
  # renderText() tells Shiny: "I will return some text for the UI".
  output$result <- renderText({
    
    # 3.2 — Read values coming from the UI
    value <- input$value    # numeric value entered by the user
    unit  <- input$unit     # option selected in the dropdown
    
    # 3.3 — Apply the correct formula depending on the selected conversion
    if (unit == "Celsius to Kelvin") {
      
      # Kelvin = Celsius + 273.15
      result <- value + 273.15
      
      # 3.4 — Return a human-readable sentence
      paste(value, "°C =", result, "K")
      
    } else {
      
      # Celsius = Kelvin - 273.15
      result <- value - 273.15
      
      paste(value, "K =", result, "°C")
    }
  })
}


###########################################################
# STEP 4 — RUN THE APP
###########################################################
# shinyApp() connects the UI and the server and launches the app.
# Run this script (for example, as app.R in RStudio) and the app opens.
shinyApp(ui = ui, server = server)
