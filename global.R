


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
    range = c(22:26,28,29,31,53,54,63) 
  ), 
  
  Group_2 = list( 
    range = c(64,65,73,74,88,91,100:103)
  ), 
  
  Group_3 = list( 
     range = c(155,158,166,169,170,181,183,206,247,250)
  ), 
  
  Group_4 = list( 
    range = c(259:263, 144:147,138)
  ), 
  
  Group_5 = list(
    range = c(268,223,227,215,213,175,150,90,10,256)
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
options(scipen=1000000000)

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

