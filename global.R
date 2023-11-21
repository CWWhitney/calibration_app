


library(googlesheets4)
library(pins)
library(fs)
library(purrr)

# User-Defined Parameters -------------------------------------------------

# Define the language questions will be asked in
language <- "Vietnamese"

# Define Google API authentication type
# If the Google Sheet is public, simply call `googlesheets4::gs4_death()` here
# to indicate that no authentication is necessary
googlesheets4::gs4_deauth()

# Define the URL of the Google Sheet
google_sheets_url <- "https://docs.google.com/spreadsheets/d/1yTboPXmDMF43YmjsuEH7bbPwcEj4fPfBD68rNWrOPSI/edit?usp=sharing"

#User interface language
User_interface_language_url<-"https://docs.google.com/spreadsheets/d/1zN3oSg_uPaKbAw-EdhJ6AzmINQLvf9FXN2IPwg81WbY/edit?usp=sharing"
interface_languages<-read_sheet(User_interface_language_url)
selected_language<-pull(interface_languages[,language],language)
# Select the questions you want for each group (i.e., each "round")
#### Change and use these to set up each workshop
selected_questions <- list(
  Group_1 = list(
    range = c(1,2,3,11,12, 27,28, 34, 35, 38) 
  ), 
  
  Group_2 = list( 
    range = c(39, 195, 42, 43, 44,45,46,47,48,49)
  ), 
  
  Group_3 = list( 
    range = c(50, 51, 52, 53, 54, 55, 56, 57, 58, 59)
  ), 
  
  Group_4 = list( 
    range = c(60, 62, 63, 64, 65, 66, 67, 68, 71, 72)
  )#, 
  
  #Group_5 = list(
  #  range = c(242:251)
  #),
  #Group_6 = list(
  #  range = c(252:261)
  #)
)
# Connect to the {pins} board for this workshop
# this is on Rstudio Connect 
#board <- pins::board_temp() # auth = "auto", "manual", "envvar", "rsconnect"
board<-pins::board_rsconnect()

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

