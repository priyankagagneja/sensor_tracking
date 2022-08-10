library(googlesheets4)
library(janitor)
library(dplyr)

source("R/utils.R")

# load("sensor_tracking_data.RData")
# print(ls())

###### Uncomment this when the gmail starts working
maintenance_log_file_name <- "https://docs.google.com/spreadsheets/d/1ZyrpyW0jtHq8GMJcuhAuwsDMxr40Pu3yJTEEtwALdiA/edit#gid=0"
history_file_name <- "https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0"

maintenance_log_raw <- read_sheet(maintenance_log_file_name, sheet = 1, col_types = "c")
maintenance_log_df <- maintenance_log_raw %>%
  clean_names()  %>%
  format_date_columns_as_date() %>%
  mutate(row_index = as.double(row_index %>% unlist())) %>%
  # clean_dates() %>%
  # mutate(row_index = row_number(), .before = "date") %>%
  identity()

history_raw <- read_sheet(history_file_name, sheet = "History_long", col_types = "c")
history_df <- history_raw %>%
  clean_names() %>%
  format_date_columns_as_date() %>%
  #clean_dates() %>%
  # mutate(current_location = if_else(is.na(host_name), site_code, host_name)) %>%
  filter(!is.na(id)) # %>%
  # select(-row_index)

history_columns_type <- read_sheet(history_file_name, sheet = "column_datatype")

numeric_type_df <- history_columns_type %>%
  filter(column_type %in% c('integer','numeric'))

history_columns_values <- read_sheet(history_file_name, sheet = "column_values_long")

###### uncomment upto here

# dashboard_df <- read_sheet("https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0", sheet = 1)
# dashboard_df <- dashboard_df[-38,-11]   # Removing the row with missing lat/long; and removing the blank column (11th column by position)
#
# dashboard_cleaned_df <- dashboard_df %>%
#     clean_names() %>%
#     clean_dates()

