


library(googlesheets4)
library(pins)
library(fs)
library(purrr)

# User-Defined Parameters -------------------------------------------------

# Define the language questions will be asked in
language <- "English"

# Define Google API authentication type
# If the Google Sheet is public, simply call `googlesheets4::gs4_death()` here
# to indicate that no authentication is necessary
googlesheets4::gs4_deauth()

# Define the URL of the Google Sheet
google_sheets_url <- "https://docs.google.com/spreadsheets/d/1yTboPXmDMF43YmjsuEH7bbPwcEj4fPfBD68rNWrOPSI/edit?usp=sharing"

# Select the questions you want for each group (i.e., each "round")
#### Change and use these to set up each workshop
selected_questions <- list(
  Group_1 = list(
    binary = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19), 
    range = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  ), 
  
  Group_2 = list(
    binary = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), 
    range = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  ), 
  
  Group_3 = list(
    binary = c(21, 23, 25, 27, 29, 31, 33, 35, 37, 39), 
    range = c(21, 23, 25, 27, 29, 31, 33, 35, 37, 39)
  ), 
  
  Group_4 = list(
    binary = NULL, 
    range = c(40:59)
  ), 
  
  Group_5 = list(
    binary = NULL, 
    range = c(60:79)
  )
)

# Connect to the {pins} board for this workshop
# this is on Rstudio Connect 
board <- pins::board_rsconnect(auth = "envvar")


# DO NOT EDIT CODE BELOW THIS LINE ----------------------------------------

# Load custom functions
fs::dir_ls("R") %>% 
  purrr::map(~ source(.x)) %>% 
  purrr::quietly()

# Import the binary & range questions from the Google Sheets
questions <- get_data(
  selected_questions_list = selected_questions,
  language = language,
  gs_url = google_sheets_url
)
# this can be used to not ping the google sheets api
# a spoecial set of files
# questions <- readRDS("data/gs_data.RDS")

# Build the UI elements for each binary question
binary_ui <- build_ui(
  questions = questions$binary, 
  type = "binary"
)

# Build the UI elements for each range question
range_ui <- build_ui(
  questions = questions$range, 
  type = "range"
)

