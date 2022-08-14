#
#   Sensor Tracking App - ILK Labs
#

library(shiny)
library(googlesheets4)
library(tidyverse)
library(janitor)
library(leaflet)
library(timevis)
library(dplyr)
library(tidyr)
library(lubridate)
library(shinythemes)


maintenance_log_columns <- c("id", "sensor_location", "issue", "resolution", "cause")
history_columns <- c("id", "sensor_current_status", "sensor_location", # "current_location",
                      "site_start_date", "site_end_date")

maintenance_log_columns_to_display <- c("ID ", "Location", "Issue", "Resolution", "Cause")
history_columns_to_display <- c("ID", "Current Status", "Location", "Start Date", "End Date")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    img(src="ILK_Logo.jpg", height="5%", width="5%", align="right", alt = "ILK Logo"),

    titlePanel("Low Cost Sensor Management Portal"),

    fluidRow(
        # strong("Sensor Selection"),
        column(width = 4, selectInput("sensor_current_location", "By Current Location", choices = NULL)),
        column(width = 4, selectInput("sensor_id", "By ID", choices = NULL)),
        column(width = 4, strong("Sensor Selection")) # , style='border-left: 3px solid;')
    ),
    fluidRow(
        column(width = 4, offset = 0, actionButton("clear_location", 'Clear')),
        column(width = 4, offset = 0, actionButton("clear_id", 'Clear')),
        column(width = 3, offset = 0, # strong("Sensor Selection"),
               verbatimTextOutput("selectionText")) # , style='border-left: 3px solid;')
        ),

    fluidRow(
        # h4("Current status of the selected Sensor"),
        # verbatimTextOutput("selectionText"),
        hr(),

        column(4,
               tabsetPanel(id = "mapPanel",
                           tabPanel(title = "Map",
                                    leafletOutput("map") # ,height="30vh",width="80vh")
               ))),
        column(8,
            tabsetPanel(id = "mainPanel",

                tabPanel(title = "History",
                         hr(),
                         actionButton("add_history", "Add History"),
                         hr(),
                         DT::dataTableOutput("history")),

                tabPanel(title = "Timeline",
                         hr(),
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
        updateSelectInput(session, "sensor_current_location",  choices = get_choices(history_df, "sensor_location"))
    })

    # Clear Selections
    observeEvent(input$clear_id, {
        clear_input(session, "sensor_id", get_choices(history_df, "id"))
        #updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(history_df$id)), selected = 'No Selection')
    })
    observeEvent(input$clear_location, {
        clear_input(session, "sensor_current_location", get_choices(history_df, "sensor_location"))
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
      # location_code <- if(input$sensor_current_location  != 'No Selection') toupper(substr(trimws(input$sensor_current_location),1,4)) else 'No Selection'
      # print(location_code)
      # vec_id <- if(input$id != 'No Selection') history_df$id == input$id else TRUE
      # vec_location <- if(input$sensor_current_location != 'No Selection') paste("history_df$site_code ==", location_code) else TRUE
      # vec_date_ge <- create_filter_vec()

        vec_id <- create_filter_vec(history_df, id, input$sensor_id)
        vec_location <- create_filter_vec(history_df, sensor_location, input$sensor_current_location) #location_code)

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
                # select(sensor_id = id, sensor_location, start = site_start_date, end = site_end_date) %>%
                # mutate(id = seq(1:nrow(.)),
                #        end = as.Date(end)) %>%
                transmute(sensor_id = id,
                          id = seq(1:nrow(.)),
                          sensor_location = sensor_location,
                          start = as.Date(site_start_date),
                          end = as.Date(site_end_date)
                          ) %>%
                unite("id_and_location", c(sensor_id, sensor_location), remove = FALSE) %>%
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
                    select(id, content = sensor_location, start, end)
            } else {
                df <- data %>%
                    select(id, content = id_and_location, start, end)
            }
        }

        print("history_timeline_data =")
        print(df)
        # print(df %>% distinct())
        return(df)
    })

    leaflet_map <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(data = history_df, lng = ~ as.numeric(longitude), lat = ~ as.numeric(latitude),
                         popup= ~ paste(id, sensor_location))

    map <- reactive({

        if(input$sensor_current_location %in% 'No Selection' & input$sensor_id %in% 'No Selection'){
            leaflet_map <- leaflet_map
        } else {
            req(selected_history_data())
            leaflet_map <- leaflet_map %>%
                addCircleMarkers(data = selected_history_data(), lng = ~ as.numeric(longitude), lat = ~ as.numeric(latitude),
                                 color = "red", popup = ~ paste(id, sensor_location))
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
        rctv <- save_record_to_df(history_df, action = "append" , session, nrow(history_df)+1)
        Sys.sleep(3)

        # Adding some data validation for numeric inputs
        error_type_df <- numeric_type_df %>%
          mutate(isValid = is.na(as.numeric(rctv[[column_name]]))) %>%
          filter(is.na(isValid))

        validate(need(error_type_df,
                      message = paste("Column(s)", error_type, "needs to contain a numeric value")))
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
        rctv <- save_record_to_df(maintenance_log_df, action = "append" , session, nrow(maintenance_log_df)+1)
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
        rctv <- save_record_to_df(maintenance_log_df, action = "update" , session, modal_df$row_index)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Saving df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "update", maintenance_log_file_name, rctv)
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
        rctv <- save_record_to_df(history_df, action = "update" , session, modal_df$row_index)
        Sys.sleep(3)
        removeModal()

        showModal(modalDialog("Saving df into the googlesheet..", footer = NULL, fade = TRUE))
        save_df_to_gsheet(action = "update", history_file_name, rctv)
        removeModal()
    })

    # setting default values for the reactive elements
    output$selectionText <- renderText(text()) #text_reactive$text)

    output$history <- DT::renderDataTable({
        validate(need(nrow(selected_history_data()) > 0, "Filtered data will show up on selection"))

        if(! is.null(selected_history_data())){
            df <- selected_history_data() %>%
                    select(all_of(history_columns))
            names(df) <- history_columns_to_display
            df

            }
        }, selection = 'single'
        , rownames= FALSE
        , options = list(
            paging =TRUE,
            pageLength =  5
        )
        )

    output$history_timeline <- renderTimevis({
        validate(need(nrow(history_timeline_data()) > 0, "Filtered data will show up on selection"))

        if(! is.null(history_timeline_data())){
            timevis(history_timeline_data() ,# %>% distinct(),
                    showZoom = TRUE)
        }
            })

    output$map <- renderLeaflet(map())

    output$maintenance_log <- DT::renderDataTable({
        validate(need(nrow(selected_maintenance_log_data()) > 0, "Filtered data will show up on selection"))

        if(! is.null(selected_maintenance_log_data())){
            df <- selected_maintenance_log_data() %>%
                select(all_of(maintenance_log_columns))

            names(df) <- maintenance_log_columns_to_display
            df
        }
    }  , selection = 'single'
    , rownames= FALSE
    , options = list(
        paging =TRUE,
        pageLength =  5
    )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
