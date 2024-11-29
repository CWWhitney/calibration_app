### CALIBRATION APP
### UNIVERSITY OF BONN
### DEVELOPED BY: KETCHBROOK ANALYTICS (MTHOMAS@KETCHBROOKANALYTICS.COM)


# 1.0 SETUP ----

## 1.1 Load Packages ----
library(shiny)
library(shinyjs)
library(shinyWidgets)   # formatting sliderInput() widgets
library(bslib)  # Bootstrap formatting 
library(glue)   # convenient string pasting
library(waiter)   # loading screens
library(reactable)   # interactive tables
library(dplyr)   # general data prep
library(purrr)   # working with lists
library(tidyr)   # more data prep
library(echarts4r)   # interactive charts
library(pins)
library(googlesheets4)
library(fs)


## 1.1 Setup Environment ----
# Run "global.R" script to load shared objects across all sessions
source("global.R")

# Create a data frame that indexes all of the workshop questions
question_index <- questions %>% 
  purrr::map_dfr(
    ~ dplyr::select(.x, Group, QuestionNumber), 
    .id = "QuestionType"
  ) %>% 
  dplyr::mutate(Group = as.integer(stringr::str_sub(Group, -1, -1))) %>% 
  dplyr::arrange(Group, QuestionType, QuestionNumber) %>% 
  dplyr::mutate(Index = dplyr::row_number())


## 1.2 Build UI Theme ----
# Develop the Bootstrap theme for the app
app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  bg = "#153015", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00"   # Bonn yellow
)


# 2.0 UI ----
ui <- shiny::navbarPage(
  
  ## 2.1 Set Up Global UI Elements ----
  title = selected_language[1],
  
  theme = app_theme,
  
  collapsible = TRUE, 
  
  # Ensure tickmark text on "Confidence" sliders is white  
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet", 
      type = "text/css", 
      href = "styling.css"
    )  
  ), 
  # 
  # # Enable use of {waiter} package 
  # waiter::use_waiter(), 
  # 
  # # Set the color for all 'inputSlider()' widgets to "Bonn Yellow"
  # shinyWidgets::chooseSliderSkin(
  #   skin = "Shiny",
  #   color = "#FBBA00"
  # ),
  
  ## 2.2 "Questions" Page ----
  mod_questions_page_ui(
    id = "questions_page",
    tab_title = selected_language[2],
    next_btn_label = selected_language[3],
    binary_results_panel_title = selected_language[4],
    range_results_panel_title = selected_language[5]
  ), 
  
  ## 2.5 "Metrics" Page ----
  mod_metrics_page_ui(
    id = "metrics_page",
    tab_title = selected_language[6],
    left_column_header = selected_language[7],
    right_column_header = selected_language[8],
    text_center = selected_language[9]
  ), 
  
  ## 2.6 "Help" Page ----
  mod_help_page_ui(
    id = "help_page",
    tab_title =  selected_language[10]
  ),
  
  footer = tagList(
    verbatimTextOutput("dev_mode")
  )
)



# 3.0 SERVER ----
server <- function(input, output, session) {
  
  
  # Session messages -----------------------------------------------------------
  
  session_start_time <- Sys.time()
  message(
    session_start_time,
    " Session ",
    session$token, 
    " started for Application ",
    application_token
  )
  active_session_counter <<- active_session_counter + 1
  
  message(
    Sys.time(),
    " Application ",
    application_token, 
    ": ",
    active_session_counter, " active sessions."
  )
  
  session$onSessionEnded(
    function() {
      session_end_time <- Sys.time()
      message(
        session_end_time,
        " Session ",
        session$token, 
        " ended for Application ",
        application_token
      )
      message(
        Sys.time(), 
        " Session ",
        session$token, 
        " lasted ", 
        format(session_end_time - session_start_time, digits = 2),
        " for Application ",
        application_token
      )
      
      active_session_counter <<- 
        active_session_counter - 1
      
      message(
        Sys.time(),
        " Application ",
        application_token, 
        ": ",
        active_session_counter, " active sessions."
      )
    })
  
  
  output$dev_mode <- renderPrint({
    reactiveValuesToList(input)
  })
  
  observe({
    stringr::str_c(
    "?session=", session$token,
    "&user_first_name=", user_info_reactives$user_first_name(),
    "&user_last_name=", user_info_reactives$user_last_name()
    ) %>% 
    updateQueryString(mode = "push")
  })
  
  
  # Language Selection ------------------------------------------------------
  
  
  # Setup initial reactiveValues
  rctv <- shiny::reactiveValues(
    
    # Create the reactive data frame holding user's "binary" question responses
    binary_tbl = data.frame(
      Group = as.integer(), 
      Question = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Response = as.character(), 
      Confidence = as.character(), 
      Truth = as.character(), 
      Brier = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    ), 
    
    # Create the reactive data frame holding user's "range" question responses
    range_tbl = data.frame(
      Group = as.integer(), 
      Question = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Lower90 = as.numeric(), 
      Upper90 = as.numeric(), 
      Truth = as.numeric(), 
      RelativeError = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    )
  )
  
  ## 3.2 User Info Modal ----
  # On app launch, display pop-up modal for user to enter first & last name
  user_info_reactives <- 
    mod_user_info_modal_server(
      id = "user_info_modal",
      modal_dialog_title = selected_language[11],
      user_first_name_label = selected_language[12], 
      user_last_name_label = selected_language[13],
      submit_user_info_btn_label = selected_language[14],
      question_index = question_index,
      rctv = rctv
    )
  
  
  ## 3.7 Render Question & Response UI  ----
  mod_questions_page_server(
    "questions_page",
    current_question_type = user_info_reactives$current_question_type, 
    current_group_number = user_info_reactives$current_group_number, 
    current_question_number = user_info_reactives$current_question_number,
    question_index = question_index,
    rctv = rctv
  )
  
  mod_metrics_page_server(
    id = "metrics_page",
    rctv = rctv
  )
  
  
}

shinyApp(ui, server, onStart = app_start_up_function)
