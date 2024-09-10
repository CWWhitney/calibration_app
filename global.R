


library(googlesheets4)
library(pins)
library(fs)
library(purrr)

# User-Defined Parameters -------------------------------------------------

# Define the language questions will be asked in
language <- "German"

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
    range = c(1, 2, 4, 7, 8, 11, 14, 17, 19, 30) 
  ), 
  
  Group_2 = list( 
    range = c(39, 46, 50, 64, 67, 85, 93, 97, 100, 285)
  ), 
  
  Group_3 = list( 
    range = c(104, 136, 138, 140, 144, 152, 154, 156, 160, 162)
  ), 
  
  Group_4 = list( 
    range = c( 170, 173, 174, 183, 208, 213, 214, 219, 221, 225)
  ), 
  
  Group_5 = list(
    range = c(242, 246, 250, 253, 259, 261, 262, 271, 274, 283)
  )#,
  #Group_6 = list(
  #  range = c(112, 114, 115, 116, 117, 118, 119, 120, 121, 122)
  #),
  #Group_7 = list(
  #  range = c(123, 124, 125, 126, 127, 128, 129, 130, 131, 132)
  #),
  #Group_8 = list(
  #  range = c(133, 135, 152, 158, 159, 166, 219, 227,291, 292, 294, 295, 300, 301, 302)

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

