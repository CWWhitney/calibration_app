


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
    binary = c(138:147),
    range = c(153:162) 
  ), 
  Group_2 = list(
    binary = c(148:151, 153), 
    range = c(175:182, 184, 207)
  ), 
  
  Group_3 = list(
    binary = NULL, 
    range = c(164, 165, 167:174)
  ), 
  
  Group_4 = list(
    binary = c(154:163), 
    range = c(208:227)
  ), 
  
  Group_5 = list(
    binary = c(164:173), 
    range = c(242:261)
  )
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

