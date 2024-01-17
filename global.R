


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

#User interface language
User_interface_language_url<-"https://docs.google.com/spreadsheets/d/1zN3oSg_uPaKbAw-EdhJ6AzmINQLvf9FXN2IPwg81WbY/edit?usp=sharing"
interface_languages<-read_sheet(User_interface_language_url)
selected_language<-pull(interface_languages[,language],language)
# Select the questions you want for each group (i.e., each "round")
#### Change and use these to set up each workshop
selected_questions <- list(
  Group_1 = list(
    range = c(1, 3, 7, 11, 12, 13, 17, 20, 25, 27) 
  ), 
  
  Group_2 = list( 
    range = c(28, 29, 31, 34, 36, 37, 39, 40, 41, 43)
  ), 
  
  Group_3 = list( 
    range = c(44, 45, 46, 47, 48, 49, 50, 51, 52, 53)
  ), 
  
  Group_4 = list( 
    range = c(56, 57, 61, 62, 63, 65, 69, 71, 72, 76)
  ), 
  
  Group_5 = list(
    range = c(86, 91, 92, 93, 99, 100, 103, 106, 110, 111)
  ),
  Group_6 = list(
    range = c(112, 114, 115, 116, 117, 118, 119, 120, 121, 122)
  ),
  Group_7 = list(
    range = c(123, 124, 125, 126, 127, 128, 129, 130, 131, 132)
  ),
  Group_8 = list(
    range = c(133, 135, 152, 158, 159, 166, 219, 227,291, 292, 294, 295, 300, 301, 302)
  )
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

