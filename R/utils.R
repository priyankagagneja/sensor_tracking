
## Function to add a record - works for both dashboard/history and maintenance log -
# It's a generic function that will automatically pull the column names
# for the data frame passed as argument
add_record_modal <- function(df) {

  print(deparse(substitute(df)))
  buttonId <- if(deparse(substitute(df)) == "history_df") "append_history" else "append_m_log"
  print(buttonId)

  showModal(modalDialog(
    purrr::map(names(df),
             ~shiny::textInput(inputId = paste0("add_",.x),
                               label = .x)
             ),
  footer = tagList(
    modalButton("Cancel"),
    # actionButton("append", "Save")
    actionButton(buttonId, "Save")
  )
  ))

}

edit_record_modal <- function(df, modal_df) { #

  buttonId <- if(deparse(substitute(df)) == "history_df") "update_history" else "update_m_log"
  print(buttonId)

  print("x")
  showModal(modalDialog(
    purrr::map(names(df),
               ~shiny::textInput(inputId = paste0("mod_",.x),
                                 label = .x,
                                 value = modal_df[,.x])
    ),
    footer = tagList(
      modalButton("Cancel"),
      # actionButton("append", "Save")
      actionButton(buttonId, "Save")
    )
  ))

  print("y")

}

record_data_modal <- function(action, data_from, modal_df) { #

  buttonId <- paste(action, data_from, sep="_")
  print(buttonId)

  df <- get(glue::glue("{data_from}_df"))

  showModal(modalDialog(
    purrr::map(names(df),
               ~shiny::textInput(inputId = paste0(action,"_",.x),
                                 label = .x,
                                 value = if(is.null(modal_df)) "" else modal_df[,.x])
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(buttonId, "Save")
    )
  ))

}


save_record_to_df <- function(df, action, session){

  # action - can be 'add' or 'mod'
  result <- tibble(input_name = paste0(action, "_", names(df))) %>%
    mutate(input_value = map_chr(input_name, ~session$input[[.x]])) %>%
    column_to_rownames(var = 'input_name') %>%
    t() %>%
    data.frame() %>%
    tidyr::unnest(cols = c()) %>%
    identity()

  # result %>% View()
  names(result) <- names(df)

  print(result)
  return(result)
}

save_df_to_gsheet <- function(destination_file, source_df){

  print("before")
  sheet_append(destination_file, source_df, sheet = 1)
                 # switch(destination_file,
  #                                                  history_file_name = "History_Long",
  #                                                  maintenance_log_file_name = "Sheet 1"
  # )

  print("after")

}

## Function to clear selection of a selectInput field.
clear_input <- function(session, id, choices) {
  updateSelectInput(session, id, choices = choices, selected = 'No Selection')
}

## Function to filter data based on what selectInput fields are selected.
# Returns a logical vector (containing true false) depending on if the condition is satisfied or not.
create_filter_vec <- function(df, var, value) {
  vec <- df %>% select({{var}})
  if(value != 'No Selection') (vec == value) else TRUE
}

filter_data <- function(df,...){
  df %>%
    filter(...)
}

filter_data2 <- function(df,var_name, var_value = ""){

  if(!length(var_value) == 0){
    df <-  df %>%
      filter({{var_name}} == var_value)
  }

  print(deparse(substitute(var_name)))
  print(var_value)
  print(length(var_value))

  return(df)
}


# clean_dates <- function(df) {
#   df %>%
#     mutate(across(contains("date"), ~ as.Date(as.character(.x), format = "%m-%d-%Y")))
# }


get_choices <- function(df, var) {
  # df_name <- deparse(substitute(df))
  data <- df %>% clean_names()

  return(switch(var,
         id = c('No Selection',unique(data$id)),
         current_location = c('No Selection',unique(data$current_location)),
         NULL)
         )


}

# NOT WORKING
get_selections <- function(df, var) {
  data <- df %>% clean_names()

  if(input$sensor_current_location %in% 'No Selection'){
    location_str = NULL
  }

  if(input$sensor_id %in% 'No Selection'){
    location_id = NULL
  }

  final_selection <- case_when(
    is.null(location_str) & is.null(location_str)  ~ "",
    is.null(location_str) & is.null(location_str)  ~ "",
  )

  # return(switch(var,
  #               id = c('No Selection',unique(data$id)),
  #               current_location = c('No Selection',unique(data$current_location)),
  #               NULL)
  # )
}
