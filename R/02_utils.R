

library(shiny)
library(googlesheets4)
library(tidyverse)
library(janitor)

## Function to add a record - works for both dashboard/history and maintenance log -
# It's a generic function that will automatically pull the column names
# for the data frame passed as argument
record_data_modal_base <- function(action, data_from, modal_df) {

  buttonId <- paste(action, data_from, sep="_")
  print(buttonId)

  df <- get(glue::glue("{data_from}_df"))  # column names from raw are not working... TO DO ... :|

  # cols_vec <- names(df)[!names(df) %in% c("row_index")]   # don't show row_index in the pop_up

  showModal(modalDialog(
    purrr::map(names(df), # cols_vec,
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

record_data_modal_basev2 <- function(action, data_from, modal_df) {

  buttonId <- paste(action, data_from, sep="_")
  print(buttonId)

  df <- get(glue::glue("{data_from}_df"))  # column names from raw are not working... TO DO ... :|

  # cols_vec <- names(df)[!names(df) %in% c("row_index")]   # don't show row_index in the pop_up

  if(data_from == "history")
  {
    choices_as_list <- history_columns_values %>%
      filter(!column_name %in% c("row_index")) %>% # remove row_index column from the pop-up
      group_by(column_name) %>%
      nest()

    # print(history_columns_values$column_name)
    # print(choices_as_list$data[1])
    # print(choices_as_list$data[choices_as_list$column_name == "sensor_owner"])
    # print(choices_as_list[[2]][[1]])
    print(choices_as_list[[1]])

    showModal(modalDialog(
      purrr::map2(.x = choices_as_list[[1]] , #choices_as_list$column_name, # col_names, # cols_vec,
                  .y = choices_as_list[[2]], # choices_as_list$data,
                  .f = #print(paste(.x, .y %>% unnest()))
                    # function(a, b) print(paste(a, b %>% unnest()))
                    function(a, b)
                      if(is.na(b$column_values) & length(b) == 1){

                        if(grepl("date",a)){
                          shiny::dateInput(inputId = paste0(action,"_",a),
                                  label = a#,
                                  #value = lubridate::today()
                                  )
                        } else {
                          shiny::textInput(inputId = paste0(action,"_",a),
                                      label = a,
                                      value = if(is.null(modal_df)) "" else modal_df[,a])
                        }
                      } else{
                      shiny::selectInput(inputId = paste0(action,"_",a),
                                   label = a,
                                   choices = b)
                      }
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(buttonId, "Save")
      )
    ))

  } else {
  showModal(modalDialog(
    purrr::map(names(df), # cols_vec,
               ~ if(grepl("date",.x)){
                 shiny::dateInput(inputId = paste0(action,"_",.x),
                                  label = .x
                 )
               } else {
                 shiny::textInput(inputId = paste0(action,"_",.x),
                                 label = .x,
                                 value = if(is.null(modal_df)) "" else modal_df[,.x])
               }
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(buttonId, "Save")
    )
  ))
  }

}

create_modal_inputs <- function(col_nm, col_vals, action, modal_df){

    # col_vals - is a list that contains named vector column_values, if applicable
    # col_vals = if(length(inputs) == 1) NA else inputs[[2]]  # null or n/a .. need to check
  # print("creating modal inputs")
  # print(col_nm)
  # print(col_vals)

  # browser()

    if((is.na(col_vals)  ) ){ # & length(col_vals) == 1 | is.null(col_vals)

      if(grepl("date",col_nm)){
        shiny::dateInput(inputId = paste0(action,"_",col_nm),
                         label = col_nm
        )
      } else {
        shiny::textInput(inputId = paste0(action,"_",col_nm),
                         label = col_nm,
                         value = if(is.null(modal_df)) "" else modal_df[,col_nm])
      }
    } else{
      shiny::selectInput(inputId = paste0(action,"_",col_nm),
                         label = col_nm,
                         choices = col_vals)
    }

}

record_data_modal <- function(action, data_from, modal_df) {

  buttonId <- paste(action, data_from, sep="_")
  print(buttonId)

  df <- get(glue::glue("{data_from}_df"))  # column names from raw are not working... TO DO ... :|

  if(data_from == "history")
  {
    choices_as_list <- history_columns_values %>%
      mutate(column_name = stringr::str_to_lower(column_name)) %>%
      filter(!column_name %in% c("row_index")) %>% # don't show row_index in the pop_up
      group_by(column_name) %>%
      nest() %>%
      ungroup()

    # print(choices_as_list[[1]])
  }

  print(modal_df)
  #print(modal_df %>% select(-row_index))

    inputs_list <- switch(data_from,
                          "history" = list(col_nm = choices_as_list[[1]],
                                           col_vals = choices_as_list[[2]], # rep(NA, nrow(choices_as_list)), #
                                           action = action,
                                           modal_df = if(is.null(modal_df)) "" else modal_df %>% select(-row_index)),
                  "maintenance_log" = list(col_nm = names(df)[!names(df) %in% c("row_index")],
                                           col_vals = rep(NA, ncol(df)-1) , #NULL, #
                                           action = action,
                                           modal_df = if(is.null(modal_df)) "" else modal_df %>% select(-row_index)) ) #

    # print(inputs_list)

    showModal(modalDialog(
      purrr::pmap(.l = inputs_list, ~ create_modal_inputs(..1,
                                                          ..2,  # ~ map(choices_as_list[[2]], pluck),
                                                          action,
                                                          modal_df)), # ~ print(inputs_list)),

      footer = tagList(
        modalButton("Cancel"),
        actionButton(buttonId, "Save")
      )
    ))
}



save_record_to_df <- function(df, action, session, row_no){

  print(session$input[[paste0(action, "_", "id")]])
  # print(session$input)

  # action - can be 'add' or 'mod'
  result <- tibble(input_name = paste0(action, "_", names(df)[!names(df) %in% c("row_index")])) %>%
      mutate(input_value = map(input_name, ~session$input[[.x]], .default = NA) ) %>% #%>% as.list()) %>%
    # returning error -> x Result 1 must be a single string, not NULL of length 0
      data.frame() %>%
      column_to_rownames(var = 'input_name') %>%
      #tidyr::unnest(cols = c()) %>%
      t() %>%
      data.frame()

  # result %>% View()
  # print(class(result))
  names(result) <- names(df)[!names(df) %in% c("row_index")]

  result <- result %>%
    mutate(row_index = row_no, .before = 1)

  #print(str(result))
  print(result)

  return(result)
}

save_df_to_gsheet <- function(action, destination_file, source_df){

  print("before")
  print(head(source_df))
  # print(class(source_df))
  print(str(source_df))

  print(paste0("A",source_df$row_index+1))

  if(action == "append"){
    sheet_append(destination_file, source_df, sheet = 1)
  } else {
    print("row_index = ")
    print(source_df$row_index)
    range_write(destination_file, source_df,
                range = paste0("A",source_df$row_index+1),
                col_names = FALSE, reformat = FALSE)
  }

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

  return(if(value != 'No Selection') (vec == value) else TRUE)
}

filter_data <- function(df,...){
  df %>%
    filter(...)
}

format_date_columns_as_date <- function(df){
  df %>%
    # Those in datetime format will be converted to Date() format
    mutate(across(where(lubridate::is.POSIXt), as.Date))

    # mutate({{var}} := as.Date(strptime({{var}}, format = " %Y-%m-%d %H:%M")))

    # TODO: those that are datetime() but appearing as a list ( combination of datetime and chr)
    # mutate(across(contains("date"), ~ as.Date(.x))) %>%
  }

get_choices <- function(df, var) {
  # df_name <- deparse(substitute(df))
  data <- df %>% clean_names()

  return(switch(var,
         id = c('No Selection',unique(data$id)),
         sensor_location = c('No Selection',unique(data$sensor_location)),
         NULL)
         )


}
