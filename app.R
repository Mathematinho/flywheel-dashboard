# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(reticulate)

#use_python("/opt/venvs/r-shiny-env/bin/python", required = TRUE)

# Configure Python environment (you may need to adjust the path)
# use_python("/usr/bin/python3")  # Uncomment and adjust path as needed

# Load required libraries
library(shiny)
library(DT)
library(reticulate)
library(shinycssloaders)

# UI
ui <- fluidPage(
  titlePanel("Flywheel AI_time_analysis"),
  
 
    withSpinner(DT::dataTableOutput("sessions_table"), type = 8, color = "#0dc5c1")
)

# Server
server <- function(input, output, session) {
  
  #load grabbed.rds
  
  values <- reactiveValues()
  values$sessions_data <- readRDS("grabbed.rds")
  

  # Render sessions table
  output$sessions_table <- DT::renderDataTable({
    if(is.null(values$sessions_data)) {
      return(NULL)
    }
    
    DT::datatable(
      values$sessions_data,
      extensions = 'Buttons',
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'excel',
            text = 'Download Excel',
            filename = 'sessions_data'
          )
        )
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)