#
#   Sensor Tracking App - ILK Labs
#

library(shiny)
library(googlesheets4)
library(shinyjs)
library(janitor)
library(dplyr)


# Google login ( for the maintainer, most likely 1 time login setup)
gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Purple Air Sensor Tracking"),

        mainPanel(
            tabsetPanel(id = "mainPanel",
                tabPanel(title = "Dashboard",
                         h3("Select the Sensor"),
                         selectInput("sensor_id", "By ID", choices = NULL),
                         selectInput("sensor_current_location", "By Current Location", choices = NULL),

                         verbatimTextOutput("selectionText"),
                         dataTableOutput("dashboard")

                         ),
                tabPanel(title = "Map", dataTableOutput("maintenance_log"))
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    maintenance_log_df <- read_sheet("https://docs.google.com/spreadsheets/d/1ZyrpyW0jtHq8GMJcuhAuwsDMxr40Pu3yJTEEtwALdiA/edit#gid=0", sheet = 1) %>%
        clean_names()

    dashboard_df <- read_sheet("https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0", sheet = 1)
    dashboard_df <- dashboard_df[,-10]

    dashboard_cleaned_df <- dashboard_df %>% clean_names()

    print(names(dashboard_df))
    # print(names(dashboard_cleaned_df))

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
        df <- dashboard_df

        if(!input$sensor_current_location %in% 'No Selection'){

            updateSelectInput(session, "sensor_id",  choices = c('No Selection',unique(dashboard_cleaned_df$id)), selected = 'No Selection')

            df <- df %>%
                    filter(`Current Location` == trimws(input$sensor_current_location))

            text <- paste("Filtered for Location = ",input$sensor_current_location)
        }

        if(!input$sensor_id %in% 'No Selection'){

            updateSelectInput(session, "sensor_current_location",  choices = c('No Selection',unique(dashboard_cleaned_df$current_location)), selected = 'No Selection')

            df <- df %>%
                filter(ID == input$sensor_id)

            text <- paste("Filtered for ID = ",input$sensor_id)
        }
        return(df)
    })

    output$selectionText <- renderText(text())

    output$dashboard <- renderDataTable({
         selected_dashboard_data()
            })

    # Row selection brings up the pop up to edit the request
    observeEvent(input$dashboard_rows_selected, {

        #Get row number of row selected in the history datatable
        rowSelected <-input$dashboard_rows_selected
        print(rowSelected)

        #build modal_df, a dataframe to be used to build values in modal dialog
        modal_df <- dashboard_df[rowSelected,] %>% clean_names()

        print(names(modal_df))

        # showModal(modalDialog(
        #     useShinyjs(),
        #     h3("Update Something"),
        #
        #     div(style="display: inline-block;vertical-align:top; width: 140px;",
        #         dateInput(inputId = "mod_ID",
        #                   label = "ID",
        #                   value = modal_df$id)),
        #
        #     textAreaInput(inputId = "mod_current_location",
        #                   label = "Give more details: E.g. What metrics matter to you/Does it involve YoY comparison etc",
        #                   value = modal_df$current_location,
        #                   rows = 3),
        #
        #     selectInput(inputId = "mod_site_maintenancec_contact",
        #                 label = "Who to contact",
        #                 choices = c("Adithi","pratyush"),
        #                 selected = modal_df$site_maintenance_contact_name,
        #                 multiple = TRUE),
        #
        #     ))

        # showModal(modalDialog(
        #    useShinyjs(),
        #     h3("Maintenance Log"),
        #    div(style="display: inline-block;vertical-align:top; width: 140px;",
        #        maintenance_log_df %>%
        #            filter(purple_air_id == modal_df$id)
        #    )))

        selected_maintenance_log_df <- reactive({
            maintenance_log_df %>%
                filter(purple_air_id == modal_df$id)
        })

        output$maintenance_log <- renderDataTable({
            selected_maintenance_log_df()
        })

        updateTabsetPanel(session, "mainPanel", selected = "tab2") # Maintenance Log
    })


    #print(names(maintenance_log_df))

}

# Run the application
shinyApp(ui = ui, server = server)
