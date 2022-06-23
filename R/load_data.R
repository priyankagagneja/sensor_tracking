library(googlesheets4)
library(janitor)
library(dplyr)

load("sensor_tracking_data.RData")
# print(ls())

###### Uncomment this when the gmail starts working
# maintenance_log_file_name <- "https://docs.google.com/spreadsheets/d/1ZyrpyW0jtHq8GMJcuhAuwsDMxr40Pu3yJTEEtwALdiA/edit#gid=0"
# history_file_name <- "https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0"
#
# maintenance_log_df <- read_sheet(maintenance_log_file_name, sheet = 1) %>%
#   clean_names()  %>%
#   # clean_dates() %>%
#   identity()
#
# history_df <- read_sheet(history_file_name, sheet = "History_long") %>%
#   clean_names() %>%
#   #clean_dates() %>%
#   mutate(current_location = if_else(is.na(host_name), site_code, host_name)) %>%
#   filter(!is.na(id)) %>%
#   select(-row_index)

###### uncomment upto here

# dashboard_df <- read_sheet("https://docs.google.com/spreadsheets/d/16qaG1P8YVaCtbPczcOBdHXGiQCuCM4KmATH5ia9OXgI/edit#gid=0", sheet = 1)
# dashboard_df <- dashboard_df[-38,-11]   # Removing the row with missing lat/long; and removing the blank column (11th column by position)
#
# dashboard_cleaned_df <- dashboard_df %>%
#     clean_names() %>%
#     clean_dates()

