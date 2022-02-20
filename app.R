#
#   Sensor Tracking App - ILK Labs
#

library(shiny)
library(googlesheets4)
library(shinyjs)
library(janitor)
library(dplyr)
library(leaflet)


# Google login ( for the maintainer, most likely 1 time login setup)
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

dashboard_cleaned_df <- dashboard_df %>% clean_names()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Purple Air Sensor Tracking"),

        mainPanel(
            tabsetPanel(id = "mainPanel",
                tabPanel(title = "Dashboard",
                         h4("Select the Sensor"),
                         selectInput("sensor_id", "By ID", choices = NULL),
                         selectInput("sensor_current_location", "By Current Location", choices = NULL),

                         actionButton("show_history", "Show History"),
                         # actionButton("edit_history", "Edit History"),

                         actionButton("show_m_log", "Show Maintenance Log"),
                         # actionButton("edit_m_log", "Edit Maintenance Log"),

                         h4("Current status of the selected Sensor"),
                         verbatimTextOutput("selectionText"),
                         dataTableOutput("dashboard"),
                         leafletOutput("map")

                         ),
                tabPanel(title = "History",
                         dataTableOutput("history")),
                tabPanel(title = "Maintenance Log",
                         DT::dataTableOutput("maintenance_log"))
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # print(names(dashboard_df))

    observe({
        updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(dashboard_cleaned_df$id)))
        updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(dashboard_cleaned_df$current_location)))
    })


    text <- reactive({
        txt <- "No filters selected"

        if(!input$sensor_current_location %in% 'No Selection'){
            txt <- paste("Filtered for Location = ",input$sensor_current_location)
        }

        if(!input$sensor_id %in% 'No Selection'){
            txt <- paste("Filtered for ID = ",input$sensor_id)
        }

        return(txt)
    })

    selected_dashboard_data <- reactive({

        if(!input$sensor_current_location %in% 'No Selection'){
            updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(dashboard_cleaned_df$id)), selected = 'No Selection')
            df <- dashboard_df %>%
                    filter(`Current Location` == trimws(input$sensor_current_location))
        }
        if(!input$sensor_id %in% 'No Selection'){
            updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(dashboard_cleaned_df$current_location)), selected = 'No Selection')
            df <- dashboard_df %>%
                filter(ID == input$sensor_id)
        }
        return(df)
    })

    map <- reactive({

        leaflet_map <- leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircleMarkers(data = dashboard_df, lng = ~ Longitude, lat = ~ Latitude, popup= ~ paste(ID,`Current Location`))

        if(!input$sensor_id %in% 'No Selection'){
            req(selected_dashboard_data())
            leaflet_map %>%
                addCircleMarkers(data = selected_dashboard_data(), lng = ~ Longitude, lat = ~ Latitude,
                                 color = "red", popup = ~ paste(ID,`Current Location`))
        }

        return(leaflet_map)
    })

    # selected_map <- reactive({
    #
    #     if(!input$sensor_id %in% 'No Selection'){
    #             addCircleMarkers(data = selected_dashboard_data(), color = "red", popup = ~ paste(ID,`Current Location`))
    #     } else { identity() }
    #
    # })


    # print(names(maintenance_log_df))

    selected_maintenance_log_df <- reactive({
        maintenance_log_df %>%
            filter(id == input$sensor_id)
    })

    observeEvent(input$show_m_log, {
        updateTabsetPanel(session, "mainPanel", selected = "Maintenance Log") # Maintenance Log
    })

    observeEvent(input$show_history, {
        updateTabsetPanel(session, "mainPanel", selected = "History") # Maintenance Log
    })

    observeEvent(input$maintenance_log_rows_selected, {

        # Get row number of row selected in the datatable
        rowSelected <- input$maintenance_log_rows_selected
        print(rowSelected)

        # build modal_df, a dataframe to be used to build values in modal dialog
        modal_df <- selected_maintenance_log_df()[rowSelected,]
        print(modal_df)

        showModal(modalDialog(
            useShinyjs(),
            h3("Update Maintenance Log"),

            div(style="display: inline-block;vertical-align:top; width: 140px;",
                # dateInput(inputId = "mod_date",
                #           label = "date",
                #           value = modal_df$date)
                ),
            # textInput(inputId = "mod_purple_air_id",
            #           label = "purple air id",
            #           value = modal_df$purple_air_id),
            textInput(inputId = "mod_id",
                      label = "id",
                      value = modal_df$id),
            # textInput(inputId = "mod_code",
            #           label = "code",
            #           value = modal_df$code),
            textInput(inputId = "mod_sensor_location",
                      label = "sensor_location",
                      value = modal_df$sensor_location),
            # textInput(inputId = "mod_issue",
            #           label = "issue",
            #           value = modal_df$issue),
            # textInput(inputId = "mod_resolution",
            #           label = "resolution",
            #           value = modal_df$resolution),
            # textInput(inputId = "mod_cause",
            #           label = "cause",
            #           value = modal_df$cause),
            # textInput(inputId = "mod_time_taken_to_fix",
            #           label = "time_taken_to_fix",
            #           value = modal_df$time_taken_to_fix_the_device),
            # textInput(inputId = "mod_time_spent_overall",
            #           label = "time_spent_including_travel",
            #           value = modal_df$time_spent_including_travel),
            # textInput(inputId = "mod_notes",
            #           label = "notes",
            #           value = modal_df$notes),
            # textInput(inputId = "mod_fixing_methods_remotely_on_site_visit",
            #           label = "fixing_methods_remotely_on_site_visit",
            #           value = modal_df$fixing_methods_remotely_on_site_visit)

            footer = tagList(
                modalButton("Cancel"),
                actionButton("update", "Update")
            )
        ))
        })


    output$selectionText <- renderText(text())
    output$dashboard <- renderDataTable(selected_dashboard_data())
    output$map <- renderLeaflet(map())
    output$maintenance_log <- DT::renderDataTable({
        selected_maintenance_log_df()
    }, selection = 'single')

}

# Run the application
shinyApp(ui = ui, server = server)
