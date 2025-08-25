# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(reticulate)

# Configure Python environment (you may need to adjust the path)
# use_python("/usr/bin/python3")  # Uncomment and adjust path as needed

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Flywheel Project Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("API Connection", tabName = "connection", icon = icon("key")),
      menuItem("Projects", tabName = "projects", icon = icon("folder"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # API Connection tab
      tabItem(tabName = "connection",
              fluidRow(
                box(
                  title = "Flywheel API Configuration", status = "primary", solidHeader = TRUE,
                  width = 12,
                  
                  # API Key input
                  passwordInput("api_key", 
                                label = "Enter Flywheel API Key:",
                                placeholder = "Your Flywheel API key here..."),
                  
                  br(),
                  
                  actionButton("connect_btn", 
                               "Connect to Flywheel", 
                               class = "btn-primary",
                               icon = icon("plug")),
                  
                  br(), br(),
                  
                  # Status output
                  verbatimTextOutput("connection_status")
                )
              )
      ),
      
      # Projects tab
      tabItem(tabName = "projects",
              fluidRow(
                box(
                  title = "Available Projects", status = "success", solidHeader = TRUE,
                  width = 12,
                  
                  # Project dropdown
                  selectInput("project_dropdown",
                              label = "Select a Project:",
                              choices = NULL,
                              selected = NULL),
                  
                  br(),
                  
                  # Projects table
                  DT::dataTableOutput("projects_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    connected = FALSE,
    projects_data = NULL,
    fw_client = NULL
  )
  
  # Python setup - define the Python code as a string
  python_code <- "
import flywheel
import pandas as pd

def connect_flywheel(api_key):
    try:
        fw = flywheel.Client(api_key)
        return fw, True, 'Successfully connected to Flywheel!'
    except Exception as e:
        return None, False, f'Connection failed: {str(e)}'

def get_projects(fw_client):
    try:
        projects = fw_client.projects()
        project_data = []
        for project in projects:
            project_data.append({
                'ID': project.id,
                'Label': project.label,
                'Description': getattr(project, 'description', 'No description'),
                'Created': str(getattr(project, 'created', 'Unknown')),
                'Modified': str(getattr(project, 'modified', 'Unknown'))
            })
        return project_data, True, 'Projects loaded successfully!'
    except Exception as e:
        return [], False, f'Failed to load projects: {str(e)}'
"
  
  # Execute Python code
  py_run_string(python_code)
  
  # Connect to Flywheel when button is clicked
  observeEvent(input$connect_btn, {
    req(input$api_key)
    
    if(nchar(trimws(input$api_key)) == 0) {
      output$connection_status <- renderText("Please enter a valid API key.")
      return()
    }
    
    # Show loading message
    output$connection_status <- renderText("Connecting to Flywheel...")
    
    tryCatch({
      # Call Python function to connect
      result <- py$connect_flywheel(input$api_key)
      fw_client <- result[[1]]
      success <- result[[2]]
      message <- result[[3]]
      
      if(success) {
        values$connected <- TRUE
        values$fw_client <- fw_client
        output$connection_status <- renderText(message)
        
        # Load projects
        projects_result <- py$get_projects(fw_client)
        projects_data <- projects_result[[1]]
        projects_success <- projects_result[[2]]
        projects_message <- projects_result[[3]]
        
        if(projects_success && length(projects_data) > 0) {
          # Convert to R data frame
          df <- do.call(rbind, lapply(projects_data, function(x) {
            data.frame(
              ID = x$ID,
              Label = x$Label,
              Description = x$Description,
              Created = x$Created,
              Modified = x$Modified,
              stringsAsFactors = FALSE
            )
          }))
          
          values$projects_data <- df
          
          # Update dropdown choices
          choices <- setNames(df$ID, paste(df$Label, "-", df$ID))
          updateSelectInput(session, "project_dropdown", 
                            choices = choices,
                            selected = df$ID[1])
          
          # Switch to projects tab
          updateTabItems(session, "tabs", selected = "projects")
          
        } else {
          output$connection_status <- renderText(paste("Connected, but", projects_message))
        }
        
      } else {
        values$connected <- FALSE
        output$connection_status <- renderText(message)
      }
      
    }, error = function(e) {
      values$connected <- FALSE
      output$connection_status <- renderText(paste("Error:", e$message))
    })
  })
  
  # Render projects table
  output$projects_table <- DT::renderDataTable({
    req(values$projects_data)
    
    DT::datatable(
      values$projects_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        search = list(search = "")
      ),
      filter = 'top',
      selection = 'single'
    )
  })
  
  # Update connection status display
  output$connection_status <- renderText({
    if(!values$connected) {
      "Not connected. Please enter your API key and click Connect."
    } else {
      "Connected to Flywheel successfully!"
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)