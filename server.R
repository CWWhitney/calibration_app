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
      "?user_first_name=", user_info_reactives$user_first_name(),
      "&user_last_name=", user_info_reactives$user_last_name(),
      "&user_session=", user_info_reactives$user_session(),
      "&workshops_set=", user_info_reactives$workshop_selection(),
      "&selected_language=", input$selected_language
    )
    
    my_url_string |>
      updateQueryString(mode = "push")
  })
  
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
  
  
  questions <- reactiveVal()
  question_index <- reactiveVal()
  
  # User Info Modal ------------------------------------------------------------
  ## On app launch, display pop-up modal for user to enter first & last name
  user_info_reactives <- 
    mod_user_info_modal_server(
      id = "user_info_modal",
      modal_dialog_title_select_new_or_load = "Welcome to the Calibration App",
      new_session_button_label = "New session",
      load_session_button_label = "Load session",
      go_back_btn_label = "GO BACK",
      introduction_text_elements = stringi::stri_rand_lipsum(2) |> lapply(htmltools::p),
      modal_dialog_title_new_session = selected_language[11],
      user_first_name_label = selected_language[12], 
      user_last_name_label = selected_language[13],
      workshop_selection_label = "Select Workshop",
      user_first_name_required_label = "First name required",
      user_last_name_required_label = "Last name required",
      workshop_selection_required_label = "Workshop selection required.",
      submit_user_info_btn_label = selected_language[14],
      modal_dialog_title_load_session = "Load Session",
      load_session_text = "Please select a previous session",
      confirm_load_session_label = "Confirm selection",
      language_choices = interface_translator$get_languages(),
      language_initial_value = interface_translator$get_key_translation(),
      rctv = rctv
    )
  
  
  observeEvent(user_info_reactives$workshop_selection(),{
    
    selected_name <- user_info_reactives$workshop_selection()
    
    selected_code <- 
      load_question_sets() |>
      dplyr::filter(question_set_name == selected_name) |> 
      dplyr::pull(encrypted_question_set_code)
    
    key <- our_key
    nonce <- our_nonce
    decoded_data <- 
      selected_code |> 
      decrypt_question_index(key = key, nonce = nonce)
    
    # Extract round info
    binary_rounds <- decoded_data |>
      dplyr::filter(Type == "Binary") |>
      dplyr::select(Number, Round)
    
    range_rounds <- decoded_data |>
      dplyr::filter(Type == "Range") |>
      dplyr::select(Number, Round)
    
    
    questions(
      prepare_workshop_question_set(
        questions_full = questions_full,
        binary_rounds = binary_rounds,
        range_rounds = range_rounds,
        languages = languages
      )
    )
    
    question_index( 
      questions() |>
        purrr::map_dfr(
          ~ dplyr::select(.x, Group, QuestionNumber),
          .id = "QuestionType"
        ) |>
        dplyr::mutate(Group = as.integer(stringr::str_sub(Group, -1, -1))) |>
        dplyr::arrange(Group, QuestionType, QuestionNumber) |>
        dplyr::mutate(Index = dplyr::row_number())
    )
  })
  
  
  # Render Question & Response UI ----------------------------------------------
  mod_questions_page_server(
    "questions_page",
    user_first_name = user_info_reactives$user_first_name,
    user_last_name = user_info_reactives$user_last_name,
    user_session = user_info_reactives$user_session,
    workshop_selection = user_info_reactives$workshop_selection,
    current_question_type = reactive(rctv$current_question_type),
    current_group_number = reactive(rctv$current_group_number),
    current_question_number = reactive(rctv$current_question_number),
    question_index = question_index,
    questions = questions,
    rctv = rctv,
    selected_language = reactive({input$selected_language}),
    issue_dialog_title = selected_language[15],
    issue_dialog_text_NA = "One of the two values was not properly filled in. Check if values were filled properly and
    are properly defined.",
    issue_dialog_text_small_large = paste0(selected_language[16], selected_language[17]),
    issue_dialog_button = selected_language[18],
    round_finished_dialog_text = "The instructor has not yet enabled the next round to start. Please try again once the instructor has allowed it.",
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
        label = NULL,
        choices = interface_translator$get_languages(),
        selected = user_info_reactives$selected_language(),
        width = "120px"
      ) |> 
        tagAppendAttributes(
          class = "mb-0",
          style = "float:right;"
        )
    })
  
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
  })
}
