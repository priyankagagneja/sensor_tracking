
library(googlesheets4)

# Google login (for the maintainer, most likely 1 time login setup)

# One time for  a project
# library(googlesheets4)

# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
#
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
#
# # trigger auth on purpose --> store a token in the specified cache
# gs4_auth(cache = ".secrets")
#
# # see your token file in the cache, if you like
# list.files(".secrets/")

options(gargle_oauth_email = "pganalytics25@gmail.com", gargle_verbosity = "debug")
gargle::token_fetch(scopes = "https://www.googleapis.com/auth/userinfo.email")

# options(gargle_oauth_email = "ilkaqapp@gmail.com", gargle_verbosity = "debug")
#gargle::token_fetch(scopes = "https://www.googleapis.com/auth/userinfo.email")

# gs4_auth(cache = ".secrets", email = TRUE,
#          scopes = c("https://www.googleapis.com/auth/spreadsheets"))

# gs4_auth(cache = ".secrets", email = "ilkaqapp@gmail.com")
#gs4_auth(cache = ".secrets", email = "cstepilk@gmail.com")

gargle::token_fetch(scopes = "https://www.googleapis.com/auth/userinfo.email")
# gargle::credentials_user_oauth2()
