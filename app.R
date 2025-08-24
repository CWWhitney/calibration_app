### CALIBRATION APP
### UNIVERSITY OF BONN
### DEVELOPED BY: KETCHBROOK ANALYTICS (MTHOMAS@KETCHBROOKANALYTICS.COM)

# Setup Environment ------------------------------------------------------------
rm(list = ls())
## Run "global.R" script to load shared objects across all sessions
source("global.R")

## UI 
ui <- function() {
  div(
    shiny.i18n::usei18n(interface_translator),
    bslib::page_navbar(
      # Set Up Global UI Elements ----------------------------------------------
      title = interface_translator$t(selected_language[1]),
      theme = app_theme,
      collapsible = TRUE,
      ## Ensure tickmark text on "Confidence" sliders is white  
      shiny::tags$head(
        shiny::tags$link(
          rel = "stylesheet", 
          type = "text/css", 
          href = "styling.css"
        ),
        tags$script(
          HTML("window.onbeforeunload = function(evt) {return true;}")
        )
        # 
        # # Enable use of {waiter} package 
        # waiter::use_waiter(), 
        # 
      ), 
      
      # Questions Page -------------------------------------------------------
      mod_questions_page_ui(
        id = "questions_page",
        tab_title = interface_translator$t(selected_language[2]),
        binary_results_panel_title = interface_translator$t(selected_language[4]),
        range_results_panel_title = interface_translator$t(selected_language[5])
      ), 
      
      # Metrics Page ---------------------------------------------------------
      mod_metrics_page_ui(
        id = "metrics_page",
        tab_title = interface_translator$t(selected_language[6]),
        left_column_header = interface_translator$t(selected_language[7]),
        right_column_header = interface_translator$t(selected_language[8]),
        text_center = interface_translator$t(selected_language[9])
      ), 
      # Help Pages ------------------------------------------------------------
      # mod_help_page_ui(
      #   id = "help_page_1",
      #   tab_title =  interface_translator$t(selected_language[10]),
      #   url = "'https://www.youtube.com/embed/7P2YI9-smfU'"
      # ),
      # mod_help_page_ui(
      #   id = "help_page_2",
      #   tab_title =  interface_translator$t(selected_language[10]),
      #   url = "'https://www.youtube.com/embed/OtYAomR9pZE?si=k8jtETxukBYJvWE0'"
      # ),
      # mod_help_page_ui(
      #   id = "help_page_3",
      #   tab_title =  interface_translator$t(selected_language[10]),
      #   url = "'https://www.youtube.com/embed/3YeWSHCUh9w?si=Jb7A8CjZZkyG0l0X'"
      # ),
      # mod_help_page_ui(
      #   id = "help_page_4",
      #   tab_title =  interface_translator$t(selected_language[10]),
      #   url = "'https://www.youtube.com/embed/eKvCAZd7px8?si=y9eucgVBZXa32Ag7'"
      # ),
      # mod_help_page_ui(
      #   id = "help_page_5",
      #   tab_title =  interface_translator$t(selected_language[10]),
      #   url = "'https://www.youtube.com/embed/qwHvGh_9tRs?si=Mit1VkZ544EgMF3C'"
      # ),
      # Footer -----------------------------------------------------------------
      footer = tagList(
        hr(),
        shiny::tabsetPanel(
          ## Language Selection ------------------------------------------------
          shiny::tabPanel(
            title = "Language Selection", 
            uiOutput("language_selection_ui"),
          ), 
          
          ## Question Selection Static -----------------------------------------
          shiny::tabPanel(
            title = "Question Selection Static", 
            reactable::reactableOutput(outputId = "lalal")
          ),
          ## Question Selection ------------------------------------------------
          shiny::tabPanel(
            title = "Question Selection", 
            mod_question_selection_ui(id = "question_selection"),
          ),
          ## Binary Overview ------------------------------------------------
          shiny::tabPanel(
            title = "Binary Overview", 
            verbatimTextOutput("binary_overview"),
            verbatimTextOutput("binary_tbl_string"),
          ),
          ## Range Overview ------------------------------------------------
          shiny::tabPanel(
            title = "Range Overview", 
            verbatimTextOutput("range_overview"),
            verbatimTextOutput("range_tbl_string"),
          ),
          ## Input Overview ------------------------------------------------
          shiny::tabPanel(
            title = "Input Overview", 
            verbatimTextOutput("dev_mode"),
          )
        )
      )
    )
  )
}

## SERVER 
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
  
  
  # URL Bookmarking ------------------------------------------------------------
  
  observe({
    my_url_string <-  stringr::str_c(
      "?session=", session$token,
      "&user_first_name=", user_info_reactives$user_first_name(),
      "&user_last_name=", user_info_reactives$user_last_name(),
      "&question_selection=", encrypted_question_index(),
      "&binary_tbl=", encrypted_binary_tbl(),
      "&range_tbl=", encrypted_range_tbl()
    )
    
    message("Nchar URL:", nchar(my_url_string))
    
    my_url_string |> 
      updateQueryString(mode = "push")
  })
  
  ## Read values from state$values when we restore
  app_restored <- reactiveVal(FALSE)
  
  onRestore(function(state) {
    app_restored(TRUE)
    
    browser("Restore")
    
    ## Parse URL parameters
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$binary_tbl)) {
      rctv$binary_tbl <- decrypt_binary_table(query$binary_tbl, key = our_key, nonce = our_nonce)
    }
    
    if (!is.null(query$range_tbl)) {
      rctv$range_tbl <- decrypt_range_table(query$range_tbl, key = our_key, nonce = our_nonce)
    }
    
  })
  
  # onRestored(function(state) {
  #
  # })
  
  # Setup initial reactiveValues -----------------------------------------------
  rctv <- shiny::reactiveValues(
    
    ## Create the reactive data frame holding user's "binary" question responses
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
    
    ## Create the reactive data frame holding user's "range" question responses
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
    ),
    current_group_number = 1,
    current_question_number = 1,
    current_question_type = "binary"
  )
  
  
  
  # User Info Modal ------------------------------------------------------------
  ## On app launch, display pop-up modal for user to enter first & last name
  user_info_reactives <- 
    mod_user_info_modal_server(
      id = "user_info_modal",
      modal_dialog_title_select_new_or_load = "Welcome to the Calibration App",
      new_session_button_label = "New session",
      load_session_button_label = "Load session",
      introduction_text_elements = stringi::stri_rand_lipsum(2) |> lapply(htmltools::p),
      modal_dialog_title_new_session = selected_language[11],
      user_first_name_label = selected_language[12], 
      user_last_name_label = selected_language[13],
      user_first_name_required_label = selected_language[12],# "First name required",
      user_last_name_required_label = "Last name required",
      submit_user_info_btn_label = selected_language[14],
      modal_dialog_title_load_session = "Load Session",
      load_session_text = "Please select a previous session",
      confirm_load_session_label = "Confirm selection",
      language_choices = interface_translator$get_languages(),
      language_initial_value = interface_translator$get_key_translation(),
      question_index = question_index,
      app_restored = app_restored,
      rctv = rctv
    )
  
  
  # Render Question & Response UI ----------------------------------------------
  mod_questions_page_server(
    "questions_page",
    # current_question_type = user_info_reactives$current_question_type, 
    # current_group_number = user_info_reactives$current_group_number, 
    # current_question_number = user_info_reactives$current_question_number,
    current_question_type = reactive(rctv$current_question_type),
    current_group_number = reactive(rctv$current_group_number),
    current_question_number = reactive(rctv$current_question_number),
    question_index = question_index,
    questions,
    rctv = rctv,
    selected_language = reactive({input$selected_language}),
    issue_dialog_title = selected_language[15],
    issue_dialog_text_NA = "One of the two values was not properly filled in. Check if values were filled properly and
    are properly defined.",
    issue_dialog_text_small_large = paste0(selected_language[16], selected_language[17]),
    issue_dialog_button = selected_language[18],
    confirm_dialog_title = selected_language[23],
    confirm_dialog_button = selected_language[14],
    word_for_question = selected_language[44],
    word_for_answer = selected_language[45],
    word_for_correct = selected_language[31],
    word_for_incorrect = selected_language[32],
    word_for_confidence = selected_language[46],
    word_for_confidence_interval = selected_language[35],
    word_for_lower_bound = selected_language[33],
    word_for_upper_bound = selected_language[34],
    next_btn_label = selected_language[3],
    modal_text_1_binary = selected_language[19],
    modal_text_1_range = selected_language[20],
    modal_text_2_binary = selected_language[21],
    modal_text_2_range = selected_language[22],
    completion_dialog_title = selected_language[24],
    completion_dialog_text_1 = selected_language[25],
    completion_dialog_text_2 = selected_language[26],
    group_complete_dialog_title = selected_language[27],
    group_complete_dialog_text_1 = selected_language[28],
    group_complete_dialog_button = selected_language[29],
    group_complete_dialog_text_2 = selected_language[30]
  )
  
  
  # Render Metrics Page --------------------------------------------------------
  mod_metrics_page_server(
    id = "metrics_page",
    rctv = rctv
  )
  
  
  # Language Selection ------------------------------------------------------
  
  output$language_selection_ui <- 
    renderUI({
      req(user_info_reactives$selected_language())
      
      selectInput(
        "selected_language",
        "Language",
        choices = interface_translator$get_languages(),
        selected = user_info_reactives$selected_language()
      )
    })
  
  observeEvent(input$selected_language, {
    ## This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    ## Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)
  })
  
  
  # Question Selection ---------------------------------------------------------
  
  output$lalal <- reactable::renderReactable({
    reactable::reactable(
      questions_full$binary,
      selection = "multiple",
      borderless = TRUE,
      onClick = "select",
      theme = reactable::reactableTheme(
        backgroundColor = "#153015",
        rowSelectedStyle = list(backgroundColor = "rgb(118, 118, 118, .5)", boxShadow = "inset 2px 0 0 0 #ffa62d")
      )
    )
  })
  
  encrypted_question_index <- 
    mod_question_selection_server(id = "question_selection")
  
  
  output$binary_overview <- renderPrint({
    rctv$binary_tbl
  })
  
  encrypted_binary_tbl <- reactive({
    rctv$binary_tbl |> 
      encrypt_binary_table(key = our_key, nonce = our_nonce)
  })
  
  output$binary_tbl_string <- renderPrint({
    encrypted_binary_tbl()
  })
  
  
  
  output$range_overview <- renderPrint({
    rctv$range_tbl
  })
  
  encrypted_range_tbl <- reactive({
    rctv$range_tbl |> 
      encrypt_range_table(key = our_key, nonce = our_nonce)
  })
  
  output$range_tbl_string <- renderPrint({
    encrypted_range_tbl()
  })
  
  
  
  
  output$dev_mode <- renderPrint({
    reactiveValuesToList(input)
  })
  
  
  
  
}

shinyApp(ui, server, onStart = app_start_up_function, enableBookmarking = "url")
