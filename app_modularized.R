#
#   Sensor Tracking App - ILK Labs
#

library(shiny)
library(googlesheets4)
library(tidyverse)
library(shinyjs)
library(janitor)
library(leaflet)
library(timevis)
library(dplyr)
library(tidyr)
library(lubridate)
library(shinythemes)

# print(here::here())
# source("R/authenticate_user.R")
source("R/utils.R")
source("R/load_data.R")

maintenance_log_columns <- c("id", "sensor_location", "issue", "resolution", "cause")
history_columns <- c("id", "sensor_current_status", "host_name", "current_location",
                      "site_start_date", "site_end_date")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    img(src="ILK_Logo.jpg", height="15%", width="15%", align="right"),
    titlePanel("Low Cost Sensor Management Portal"),

    sidebarLayout(

        # Sidebar with a slider input
        sidebarPanel(
            h4("Select the Sensor"),

            selectInput("sensor_current_location", "By Current Location", choices = NULL),
            actionButton("clear_location", 'Clear'),

            selectInput("sensor_id", "By ID", choices = NULL),
            actionButton("clear_id", 'Clear')

        ),
        mainPanel(

            leafletOutput("map",height="30vh",width="80vh"),

            h4("Current status of the selected Sensor"),
            verbatimTextOutput("selectionText"),
            hr(),

            tabsetPanel(id = "mainPanel",
                # tabPanel(title = "Dashboard",
                #          DT::dataTableOutput("dashboard")),

                tabPanel(title = "History",
                         hr(),
                         # tableOutput("tbl"),
                         # hr(),
                         actionButton("add_history", "Add History"),
                         hr(),
                         DT::dataTableOutput("history"),
                         timevisOutput("history_timeline")),

                tabPanel(title = "Maintenance Log",
                         hr(),
                         actionButton("add_m_log", "Add Maintenance Log"),
                         hr(),
                         DT::dataTableOutput("maintenance_log"))
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    print(head(history_df))

    # Update Dropdowns
    observe({
        updateSelectInput(session, "sensor_id",  choices = get_choices(history_df, "id"))
        updateSelectInput(session, "sensor_current_location",  choices = get_choices(history_df, "current_location"))
    })

    # Clear Selections
    observeEvent(input$clear_id, {
        clear_input(session, "sensor_id", get_choices(history_df, "id"))
        #updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(history_df$id)), selected = 'No Selection')
    })
    observeEvent(input$clear_location, {
        clear_input(session, "sensor_current_location", get_choices(history_df, "current_location"))
        #updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(history_df$current_location)), selected = 'No Selection')
    })


    text <- reactive({
        # if(input$sensor_current_location == '' & input$sensor_id == ''){
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

    selected_history_data <- reactive({

        location_code <- if(input$sensor_current_location  != 'No Selection') toupper(substr(trimws(input$sensor_current_location),1,4)) else 'No Selection'
        #print(location_code)

        vec_id <- create_filter_vec(history_df, id, input$sensor_id)
        vec_location <- create_filter_vec(history_df, site_code, location_code)

        # vec_id <- if(input$id != 'No Selection') history_df$id == input$id else TRUE
        # vec_location <- if(input$sensor_current_location != 'No Selection') paste("history_df$site_code ==", location_code) else TRUE
        # vec_date_ge <- create_filter_vec()

        # print(vec_id)
        # print(vec_location)

        df <- history_df %>%
            filter_data(vec_id, vec_location) %>%
            identity()

        print("selected_history_data =")
        print(df)

        return(df)
    })

    history_timeline_data <- reactive({
        req(selected_history_data())
        if(! is.null(selected_history_data())){

            data <- selected_history_data() %>%
                select(sensor_id = id, site_code, start = site_start_date, end = site_end_date) %>%
                mutate(id = seq(1:nrow(.))) %>%
                unite("id_and_site_code", c(sensor_id, site_code), remove = FALSE) %>%
                mutate_at(vars(end), list(~ replace_na(., Sys.Date()))) %>%  # replace_na(end, Sys.Date()) %>%
                arrange(desc(start)) %>%
                identity()

            # print(names(data))
            print(head(data))

            if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- NULL
            } else if(!input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
                df <- data %>%
                    select(id, content = sensor_id, start, end)
            } else if(!input$sensor_id %in% 'No Selection' & input$sensor_current_location %in% 'No Selection'){
                df <- data %>%
                    select(id, content = site_code, start, end)
            } else {
                df <- data %>%
                    select(id, content = id_and_site_code, start, end)
            }
        }

        print("history_timeline_data =")
        print(df)
        # print(df %>% distinct())
        return(df)
    })

    leaflet_map <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(data = history_df, lng = ~ longitude, lat = ~ latitude, popup= ~ paste(id, current_location))

    map <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            leaflet_map <- leaflet_map
        } else {
            req(selected_history_data())
            leaflet_map <- leaflet_map %>%
                addCircleMarkers(data = selected_history_data(), lng = ~ longitude, lat = ~ latitude,
                                 color = "red", popup = ~ paste(id, current_location))
        }

        return(leaflet_map)
    })


    selected_maintenance_log_data <- reactive({

        vec_id <- create_filter_vec(maintenance_log_df, id, input$sensor_id)
        vec_location <- create_filter_vec(maintenance_log_df, sensor_location, input$sensor_current_location)

        # print(vec_id)
        # print(vec_location)

        df <- maintenance_log_df %>%
            filter_data(vec_id, vec_location) %>%
            identity()

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            df <- NULL
        }

        print("selected_maintenance_log_data =")
        print(df)
        return(df)
    })

    ### ADD BUTTONS
    observeEvent(input$add_history, {
        #append_history_record <- add_record_modal(history_df)
        append_history_record <- record_data_modal(action = "append", data_from = "history", modal_df = NULL)
    })

    observeEvent(input$append_history, {

        showModal(modalDialog("Saving new record as data frame..", footer = NULL, fade = TRUE))
        rctv <- save_record_to_df(history_df, action = "append" , session) %>%
            select(-current_location)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Saving df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "append", history_file_name, rctv)
        removeModal()

    })


    # TODO: modify the Input type for different variables/fields
    observeEvent(input$add_m_log, {
        # append_maintenance_log_record <- add_record_modal(maintenance_log_df)
        append_maintenance_log_record <- record_data_modal(action = "append", data_from = "maintenance_log", modal_df = NULL)
    })

    observeEvent(input$append_maintenance_log, {

        showModal(modalDialog("Saving new record as data frame..", footer = NULL, fade = TRUE))
        rctv <- save_record_to_df(maintenance_log_df, action = "append" , session)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Writing the df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "append", maintenance_log_file_name, rctv)
        removeModal()

    })


    #### UPDATE PROCESS ON DOUBLE CLICKING THE RECORD
    observeEvent(input$maintenance_log_rows_selected, {

        # Get row number of row selected in the datatable
        rowSelected <- input$maintenance_log_rows_selected
        print(rowSelected)

        # build modal_df, a dataframe to be used to build values in modal dialog
        modal_df <- selected_maintenance_log_data()[rowSelected,]
        print(modal_df)

        # update_maintenance_log_record <- edit_record_modal(maintenance_log_df)  # , modal_df
        update_maintenance_log_record <- record_data_modal(action = "update", data_from = "maintenance_log", modal_df)
    })

    observeEvent(input$update_maintenance_log, {
        # modify the googlesheet
        print("maintenance log record clicked for updating")

        showModal(modalDialog("Saving modified record as data frame..", easyClose = TRUE, footer = NULL, fade = TRUE))
        rctv <- save_record_to_df(maintenance_log_df, action = "update" , session)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Saving df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "update", history_file_name, rctv)
        removeModal()
    })

    observeEvent(input$history_rows_selected, {

        # Get row number of row selected in the datatable
        rowSelected <- input$history_rows_selected
        print(rowSelected)

        # build modal_df, a dataframe to be used to build values in modal dialog
        modal_df <- selected_history_data()[rowSelected,]
        print(modal_df)

        # update_history_record <- edit_record_modal(history_df, modal_df)  # , modal_df
        update_history_record <- record_data_modal(action = "update", data_from = "history", modal_df)
    })

    observeEvent(input$update_history, {
        # modify the googlesheet
        print("history record clicked for updating")

        showModal(modalDialog("Saving modified record as data frame..", easyClose = TRUE, footer = NULL, fade = TRUE))
        rctv <- save_record_to_df(history_df, action = "update" , session) %>%
            select(-current_location)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Saving df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "update", history_file_name, rctv)
        removeModal()
    })

    # setting default values for the reactive elements
    output$selectionText <- renderText(text()) #text_reactive$text)

    output$history <- DT::renderDataTable({
        if(! is.null(selected_history_data())){
            selected_history_data() %>%
                    select(all_of(history_columns))
            }
        })

    output$history_timeline <- renderTimevis({
        if(! is.null(history_timeline_data())){
            timevis(history_timeline_data() ,# %>% distinct(),
                    showZoom = TRUE)
        }
            })

    output$map <- renderLeaflet(map())

    output$maintenance_log <- DT::renderDataTable({
        if(! is.null(selected_maintenance_log_data())){
            selected_maintenance_log_data() %>%
                select(all_of(maintenance_log_columns))
        }
    }  , selection = 'single')

}

# Run the application
shinyApp(ui = ui, server = server)
