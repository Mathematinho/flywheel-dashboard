# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(reticulate)

use_python("/opt/venvs/r-shiny-env/bin/python", required = TRUE)

# Configure Python environment (you may need to adjust the path)
# use_python("/usr/bin/python3")  # Uncomment and adjust path as needed

# Load required libraries
library(shiny)
library(DT)
library(reticulate)
library(shinycssloaders)

# UI
ui <- fluidPage(
  titlePanel("Flywheel Session Explorer"),
  
  # API Key section
  wellPanel(
    textInput("api_key", 
              label = "Flywheel API Key:",
              placeholder = "Enter your API key"),
    
    actionButton("submit_api", "Connect", class = "btn-primary"),
    
    br(), br(),
    
    conditionalPanel(
      condition = "input.submit_api > 0",
      withSpinner(textOutput("connection_status"), type = 4, color = "#0dc5c1")
    )
  ),
  
  # Project dropdown
  conditionalPanel(
    condition = "output.show_projects",
    wellPanel(
      selectInput("project_dropdown",
                  label = "Select Project:",
                  choices = NULL),
      
      conditionalPanel(
        condition = "input.project_dropdown != ''",
        withSpinner(textOutput("loading_status"), type = 1, color = "#0dc5c1")
      )
    )
  ),
  
  # Sessions data table
  conditionalPanel(
    condition = "output.show_table",
    withSpinner(DT::dataTableOutput("sessions_table"), type = 8, color = "#0dc5c1")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    fw_client = NULL,
    projects_data = NULL,
    sessions_data = NULL,
    connection_status = "",
    loading_status = "",
    show_projects = FALSE,
    show_table = FALSE
  )
  
  # Python code
  python_code <- "
import flywheel
import pandas as pd

def connect_and_get_projects(api_key):
    try:
        fw = flywheel.Client(api_key)
        projects = fw.projects()
        project_list = []
        for project in projects:
            project_list.append({'id': project.id, 'label': project.label})
        return fw, project_list, True
    except Exception as e:
        return None, [], False

def get_sessions_data(fw_client, project_id):
    try:
        project = fw_client.get_project(project_id)
        sessions = project.sessions()
        
        session_data = []
        for session in sessions:
            time1 = session.label if hasattr(session, 'label') else ''
            
            # Get acquisitions for this session
            acquisitions = session.acquisitions()
            
            if acquisitions:
                for acq in acquisitions:
                    time2 = ''
                    time3 = ''
                    
                    # Get time2 and time3 if files exist
                    if acq.files:
                        try:
                            file_obj = acq.files[0].reload()
                            time2 = file_obj.info.get('InstanceCreationTime', '') if hasattr(file_obj, 'info') and file_obj.info else ''
                            time3 = str(acq.files[0].created) if hasattr(acq.files[0], 'created') else ''
                        except:
                            time2 = ''
                            time3 = ''
                    
                    session_data.append({
                        'session': session.label if hasattr(session, 'label') else '',
                        'acquisition': acq.label if hasattr(acq, 'label') else '',
                        'file': acq.files[0].name if acq.files else '',
                        'time1': time1,
                        'time2': time2,
                        'time3': time3
                    })
            else:
                # If no acquisitions, still show the session
                session_data.append({
                    'session': session.label if hasattr(session, 'label') else '',
                    'acquisition': '',
                    'file': '',
                    'time1': time1,
                    'time2': '',
                    'time3': ''
                })
        
        return session_data, True
    except Exception as e:
        return [], False
"
  
  # Execute Python code
  py_run_string(python_code)
  
  # Observe API key submit button
  observeEvent(input$submit_api, {
    req(input$api_key)
    
    if(nchar(trimws(input$api_key)) == 0) {
      values$connection_status <- "Please enter a valid API key."
      values$show_projects <- FALSE
      values$show_table <- FALSE
      return()
    }
    
    values$connection_status <- "Connecting to Flywheel..."
    values$show_projects <- FALSE
    values$show_table <- FALSE
    
    tryCatch({
      result <- py$connect_and_get_projects(input$api_key)
      fw_client <- result[[1]]
      projects_list <- result[[2]]
      success <- result[[3]]
      
      if(success && length(projects_list) > 0) {
        values$fw_client <- fw_client
        values$projects_data <- projects_list
        values$connection_status <- paste("Connected! Found", length(projects_list), "projects.")
        values$show_projects <- TRUE
        
        # Create choices for dropdown
        choices <- setNames(
          sapply(projects_list, function(x) x$id),
          sapply(projects_list, function(x) x$label)
        )
        
        updateSelectInput(session, "project_dropdown", 
                          choices = c("Select a project..." = "", choices),
                          selected = "")
      } else {
        values$connection_status <- "Connection failed. Please check your API key."
        values$show_projects <- FALSE
        values$show_table <- FALSE
      }
      
    }, error = function(e) {
      values$connection_status <- paste("Error:", e$message)
      values$show_projects <- FALSE
      values$show_table <- FALSE
    })
  })
  
  # Observe project selection and load sessions
  observeEvent(input$project_dropdown, {
    req(input$project_dropdown, values$fw_client)
    
    if(input$project_dropdown == "") {
      values$show_table <- FALSE
      return()
    }
    
    values$loading_status <- "Loading sessions and acquisitions..."
    values$show_table <- FALSE
    
    tryCatch({
      result <- py$get_sessions_data(values$fw_client, input$project_dropdown)
      sessions_data <- result[[1]]
      success <- result[[2]]
      
      if(success && length(sessions_data) > 0) {
        # Convert to R data frame
        df <- do.call(rbind, lapply(sessions_data, function(x) {
          data.frame(
            Session = x$session,
            Acquisition = x$acquisition,
            File = x$file,
            Time1 = x$time1,
            Time2 = x$time2,
            Time3 = x$time3,
            stringsAsFactors = FALSE
          )
        }))
        
        values$sessions_data <- df
        values$loading_status <- paste("Loaded", nrow(df), "records.")
        values$show_table <- TRUE
      } else {
        values$sessions_data <- NULL
        values$loading_status <- "No sessions found for this project."
        values$show_table <- FALSE
      }
      
    }, error = function(e) {
      values$sessions_data <- NULL
      values$loading_status <- paste("Error loading sessions:", e$message)
      values$show_table <- FALSE
    })
  })
  
  # Output for connection status
  output$connection_status <- renderText({
    values$connection_status
  })
  
  # Output for loading status
  output$loading_status <- renderText({
    values$loading_status
  })
  
  # Output for showing projects panel
  output$show_projects <- reactive({
    values$show_projects
  })
  outputOptions(output, "show_projects", suspendWhenHidden = FALSE)
  
  # Output for showing table
  output$show_table <- reactive({
    values$show_table
  })
  outputOptions(output, "show_table", suspendWhenHidden = FALSE)
  
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