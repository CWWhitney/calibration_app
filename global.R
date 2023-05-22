


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
    range = c(153:162) 
  ), 
  
  Group_2 = list( 
    range = c(175:181, 184, 207, 100)
  ), 
  
  Group_3 = list( 
    range = c(164, 166, 169:174, 63, 64)
  ), 
  
  Group_4 = list( 
    range = c(208:217)
  ), 
  
  Group_5 = list(
    range = c(242:251)
  ),
  Group_6 = list(
    range = c(252:261)
  )
)


# Connect to the {pins} board for this workshop
# this is on Rstudio Connect 
 board <- pins::board_rsconnect() # auth = "auto", "manual", "envvar", "rsconnect"
# board <- pins::board_temp() 


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
# a special set of files
# questions <- readRDS("data/gs_data.RDS")



# Build the UI elements for each range question
range_ui <- build_ui(
  questions = questions$range, 
  type = "range"
)

