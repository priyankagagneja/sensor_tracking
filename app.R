#
#   Sensor Tracking App - ILK Labs
#

library(shiny)
library(googlesheets4)
library(shinyjs)
library(janitor)
library(dplyr)
library(leaflet)
library(timevis)

# Google login (for the maintainer, most likely 1 time login setup)
gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
)

maintenance_log_df <- read_sheet("https://docs.google.com/spreadsheets/d/1ZyrpyW0jtHq8GMJcuhAuwsDMxr40Pu3yJTEEtwALdiA/edit#gid=0", sheet = 1) %>%
    clean_names()

dashboard_df <- read_sheet("https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0", sheet = 1)
dashboard_df <- dashboard_df[-38,-11]   # Removing the row with missing lat/long; and removing the blank column (11th column by position)

dashboard_cleaned_df <- dashboard_df %>%
    clean_names()

history_df <- read_sheet("https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0", sheet = "History_long") %>%
    clean_names() %>%
    mutate(host_location = if_else(is.na(host_name), site_code, host_name))

dashboard_display_df <- dashboard_df %>%
    select(ID, `Current Location`, `Installation Date`,
           `Removal Date`, `Site Maintenance Contact Name`,
           `Site Maintenance Contact Number`, `Site Maintenance Contact Email` )
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Purple Air Sensor Tracking"),

        mainPanel(
            h4("Select the Sensor"),
            selectInput("sensor_id", "By ID", choices = NULL),
            actionButton("clear_id", 'Clear'),
            selectInput("sensor_current_location", "By Current Location", choices = NULL),
            actionButton("clear_location", 'Clear'),

            actionButton("show_history", "Show History"),
            actionButton("show_m_log", "Show Maintenance Log"),


            leafletOutput("map"),

            h4("Current status of the selected Sensor"),
            verbatimTextOutput("selectionText"),

            tabsetPanel(id = "mainPanel",
                tabPanel(title = "Dashboard",
                         DT::dataTableOutput("dashboard")),

                tabPanel(title = "History",
                         actionButton("add_history", "Add History"),
                         DT::dataTableOutput("history"),
                         timevisOutput("history_timeline")),

                tabPanel(title = "Maintenance Log",
                         actionButton("add_m_log", "Add Maintenance Log"),
                         DT::dataTableOutput("maintenance_log"))
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # print(names(history_df))

    observe({
        updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(dashboard_cleaned_df$id)))
        updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(dashboard_cleaned_df$current_location)))
    })

    # Clear Selections
    observeEvent(input$clear_id, {
        updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(dashboard_cleaned_df$id)), selected = 'No Selection')
        })
    observeEvent(input$clear_location, {
        updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(dashboard_cleaned_df$current_location)), selected = 'No Selection')
        })

    text <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            txt <- "No filters selected"
        } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            txt <- paste("Filtered for Location = ",input$sensor_current_location)
        } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
            txt <- paste("Filtered for ID = ",input$sensor_id)
        } else {
            txt <- paste("Filtered for ID =",input$sensor_id,
                         "and Location =",input$sensor_current_location)
        }

        print(paste("text =", txt))
        return(txt)
    })

    selected_dashboard_data <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            df <- dashboard_df
        } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            df <- dashboard_df %>%
                filter(`Current Location` == trimws(input$sensor_current_location))
        } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
            df <- dashboard_df %>%
                filter(ID == input$sensor_id)
        } else {
            df <- dashboard_df %>%
                filter(ID == input$sensor_id & `Current Location` == trimws(input$sensor_current_location))
        }

        print("selected_dashboard_data =")
        print(df)
        return(df)
    })

    selected_history_data <- reactive({

        if(! is.null(history_df)){
            if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- NULL
            } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- history_df %>%
                    # filter(host_location == trimws(input$sensor_current_location))
                    filter(site_code == toupper(substr(trimws(input$sensor_current_location),1,4)))
            } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
                df <- history_df %>%
                    filter(id == input$sensor_id)
            } else {
                df <- history_df %>%
                    filter(id == input$sensor_id &
                           site_code == toupper(substr(trimws(input$sensor_current_location),1,4)))
            }
        }

        print("selected_history_data =")
        print(df)

        return(df)
    })

    history_timeline_data <- reactive({
        # req(selected_history_data())
        if(! is.null(selected_history_data())){
            if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- NULL
            } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- selected_history_data() %>%
                    select(id = seq(1:nrow(.)),
                           content = id,
                           start = site_start_date,
                           end = site_end_date)
            } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
                df <- selected_history_data() %>%
                    select(id = seq(1:nrow(.)),
                           content = site_code,
                           start = site_start_date,
                           end = site_end_date)
            } else {
                df <- selected_history_data() %>%
                    unite("id_and_site_code", c(id,site_code), remove = FALSE) %>%
                    select(id = seq(1:nrow(.)),
                           content = id_and_site_code,
                           start = site_start_date,
                           end = site_end_date)
            }
        }

        print("history_timeline_data =")
        print(df)
        # print(df %>% distinct())
        return(df)
    })

    # print(names(maintenance_log_df))
    selected_maintenance_log_data <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            df <- NULL
        } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            df <- maintenance_log_df %>%
                filter(sensor_location == trimws(input$sensor_current_location))
        } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
            df <- maintenance_log_df %>%
                filter(id == input$sensor_id)
        } else {
            df <- maintenance_log_df %>%
                filter(id == input$sensor_id & sensor_location == trimws(input$sensor_current_location))
        }

        print("selected_maintenance_log_data =")
        print(df)
        return(df)
    })


    leaflet_map <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(data = dashboard_df, lng = ~ Longitude, lat = ~ Latitude, popup= ~ paste(ID,`Current Location`))

    map <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            leaflet_map <- leaflet_map
        } else {
            req(selected_dashboard_data())
            leaflet_map <- leaflet_map %>%
                addCircleMarkers(data = selected_dashboard_data(), lng = ~ Longitude, lat = ~ Latitude,
                                 color = "red", popup = ~ paste(ID,`Current Location`))
        }

        return(leaflet_map)
    })


    # Maintenance Log Tab
    observeEvent(input$show_m_log, {
        updateTabsetPanel(session, "mainPanel", selected = "Maintenance Log")
    })

    # History Tab
    observeEvent(input$show_history, {
        updateTabsetPanel(session, "mainPanel", selected = "History")
    })

    # setting default values for the reactive elements
    output$selectionText <- renderText(text()) #text_reactive$text)
    output$dashboard <- DT::renderDataTable({
        if(! is.null(selected_dashboard_data())){
            selected_dashboard_data()  %>%
                select(ID, `Current Location`, `Installation Date`,
                       `Removal Date`, `Site Maintenance Contact Name`,
                       `Site Maintenance Contact Number`, `Site Maintenance Contact Email`)
        }
            })
    output$history <- DT::renderDataTable({
        if(! is.null(selected_history_data())){
        selected_history_data() %>%
                select(id, site_code, site_start_date, site_end_date, sensor_owner,
                       sensor_current_status, operation, site_type, site_located_at,
                       host_name, host_contact_number, height_floor_number, host_email)
            }
        })

    output$history_timeline <- renderTimevis({
        if(! is.null(history_timeline_data())){
            timevis(history_timeline_data()  %>% distinct(),
                    showZoom = TRUE)
        }
            })

    output$map <- renderLeaflet(map())   # map())
    output$maintenance_log <- DT::renderDataTable({
        selected_maintenance_log_data()
    }  , selection = 'single')

}

# Run the application
shinyApp(ui = ui, server = server)
