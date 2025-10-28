##******************************************************************************
##*
##* This is the user_info_modal shiny module which description
##*
##******************************************************************************

#' Title to be filled in ...e.g:
#'
#' Description to be filled in...
#'
#' @param id The namespace id of the module.
#'
#' @return An HTML element for use in a UI.
#'
#'
#' @seealso [mod_user_info_modal_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_user_info_modal_ui(
#'          id = "mod_user_info_modal"
#'        )
#'      ),
#'      server = function(input, output, session) {
#'        mod_user_info_modal_server(
#'          id ="mod_user_info_modal"
#'        )
#'      }
#'    )
#'  
#'  }
mod_user_info_modal_choose <- 
  function(
    id,
    modal_dialog_title,
    language_choices,
    language_initial_value,
    ...,
    new_session_button_label,
    load_session_button_label
  ) {
    ns <- NS(id)
    tagList(
      shiny::modalDialog(
        title = modal_dialog_title,
        shiny::tagList(
          shiny::selectInput(
            ns("selected_language"),
            "Language",
            choices = language_choices,
            selected = language_initial_value
          ),
          ...
        ),
        easyClose = FALSE, 
        size = "xl",
        footer = shiny::tagList(
          shiny::div(
            shiny::actionButton(ns("new_session"), new_session_button_label),
            shiny::actionButton(ns("load_session"), load_session_button_label)
          )
        )
      )
    )
  }


modal_user_info_modal_new_session_from_link <- 
  function(
    id,
    modal_dialog_title,
    user_first_name_label, 
    user_last_name_label,
    submit_user_info_btn_label
  ) {
    ns <- NS(id)
    shiny::modalDialog(
      title = modal_dialog_title,
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::textInput(
            inputId = ns("user_first_name"), 
            label = user_first_name_label,
            placeholder = user_first_name_label,
          ), 
          shiny::textInput(
            inputId = ns("user_last_name"), 
            label = user_last_name_label,
            placeholder = user_last_name_label
          )
        )
      ), 
      easyClose = FALSE, 
      size = "m",
      footer = shiny::tagList(
        shiny::div(
          ## Button to submit user's information
          shiny::actionButton(
            inputId = ns("submit_user_info_btn_from_link"), 
            label = submit_user_info_btn_label,
            icon = shiny::icon("check")
          )
        )
      )
    )
  }

modal_user_info_modal_new_session <- 
  function(
    id,
    modal_dialog_title,
    user_first_name_label, 
    user_last_name_label,
    workshop_selection_label,
    workshop_selection_choices,
    submit_user_info_btn_label,
    go_back_btn_label
  ) {
    ns <- NS(id)
    shiny::modalDialog(
      title = modal_dialog_title,
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1/2,
          shiny::textInput(
            inputId = ns("user_first_name"), 
            label = user_first_name_label,
            placeholder = user_first_name_label,
          ), 
          shiny::textInput(
            inputId = ns("user_last_name"), 
            label = user_last_name_label,
            placeholder = user_last_name_label
          )
        ),
        if(!is.null(workshop_selection_choices)) {
          bslib::layout_column_wrap(
            width = 1,
            shiny::selectInput(
              inputId = ns("workshop_selection"),
              label = workshop_selection_label,
              choices = workshop_selection_choices
            )
          )
        }
      ), 
      easyClose = FALSE, 
      size = "m",
      footer = shiny::tagList(
        shiny::div(
          ## Button to submit user's information
          shiny::actionButton(
            inputId = ns("submit_user_info_btn"), 
            label = submit_user_info_btn_label,
            icon = shiny::icon("check")
          ),
          shiny::actionButton(ns("go_back"), go_back_btn_label)
        )
      )
    )
    
  }


modal_user_info_modal_load_session <- function(
    id,
    modal_dialog_title,
    load_session_text,
    confirm_load_session_label,
    go_back_btn_label
) {
  ns <- NS(id)
  shiny::modalDialog(
    title = modal_dialog_title,
    shiny::tagList(
      shiny::div(
        shiny::p(load_session_text),
        reactable::reactableOutput(ns("table"))
      )
    ),
    easyClose = FALSE,
    size = "xl",
    footer = shiny::tagList(
      shiny::div(
        shiny::actionButton(
          inputId = ns("confirm_selection_btn"), 
          label = confirm_load_session_label,
          icon = shiny::icon("check")
        ),
        shiny::actionButton(ns("go_back"), go_back_btn_label)
      )
    )
  )
}



#' @param id The namespace id of the module.
#'
#' @return `list` of `shiny::reactive({})`s
#'
#' @seealso [user_info_modal_ui()]
#'
#' @export
#'
#' @inherit mod_user_info_modal_ui title description details examples
mod_user_info_modal_server <- function(
    id,
    modal_dialog_title_select_new_or_load,
    new_session_button_label,
    load_session_button_label,
    go_back_btn_label,
    introduction_text_element,
    modal_dialog_title_new_session,
    user_first_name_label, 
    user_last_name_label,
    workshop_selection_label,
    user_first_name_required_label,
    user_last_name_required_label,
    workshop_selection_required_label,
    submit_user_info_btn_label,
    modal_dialog_title_load_session,
    load_session_text,
    confirm_load_session_label,
    language_choices,
    language_initial_value,
    rctv,
    global_selected_language
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      selected_language <- reactiveVal()
      
      observe({
        req(input$selected_language)
        selected_language(input$selected_language)
      })
      
      
      initial_modal <- reactiveVal(TRUE)
      # App Restore ------------------------------------------------------------
      app_restored <- reactiveVal(FALSE)
      app_restored_from_link <- reactiveVal(FALSE)
      
      
      onRestore(function(state) {
        
        ## Parse URL parameters
        query <- parseQueryString(session$clientData$url_search)
        
        if(
          all(
            c(
              "user_first_name",
              "user_last_name",
              "user_session",
              "workshop_set"
            ) %in% 
            names(query)
          )
        ) {
          selected_user <- load_users_table() |> 
            filter(
              user_first_name == query$user_first_name,
              user_last_name == query$user_last_name,
              user_session == query$user_session,
              workshop_set == query$workshop_set
            )
        }  else {
          selected_user <- 
            data.frame()
        }
        
        if(nrow(selected_user) == 1) {
          
          app_restored(TRUE)
          
          user_first_name(selected_user$user_first_name)
          user_last_name(selected_user$user_last_name)
          user_session(selected_user$user_session)
          workshop_selection(selected_user$workshop_set)
          
          rctv$current_group_number <- selected_user$round_number
          rctv$current_question_number <- selected_user$question_number
          # rctv$current_question_type <- selected_user$question_type
          
          binary_responses <- load_users_binary_responses(
            pool = pool, 
            first_name = selected_user$user_first_name,
            last_name = selected_user$user_last_name,
            session = selected_user$user_session,
            workshop = selected_user$workshop_set
          ) |> 
            dplyr::select(
              Group = round_number,
              Question = question_number,
              QuestionText = question_text,
              Index = index_in_set,
              Response = response,
              Confidence = confidence,
              Truth = truth,
              Brier = brier_score
            ) |> 
            dplyr::mutate(
              Source = ""
            )
          
          rctv$binary_tbl <- rctv$binary_tbl |>
            rbind(binary_responses)
          
          range_responses <- load_users_range_responses(
            pool = pool, 
            first_name = selected_user$user_first_name,
            last_name = selected_user$user_last_name,
            session = selected_user$user_session,
            workshop = selected_user$workshop_set
          ) |> 
            dplyr::select(
              Group = round_number,
              Question = question_number,
              QuestionText = question_text,
              Index = index_in_set,
              Lower90 = lower_90,
              Upper90 = upper_90,
              Truth = truth,
              RelativeError = relative_error
            ) |> 
            dplyr::mutate(
              Source = ""
            )
          
          rctv$range_tbl <- rctv$range_tbl |>
            rbind(range_responses)
          
          
          initial_modal(FALSE)
        } else if("workshop_set" %in% names(query)) {
          app_restored_from_link(TRUE)
          workshop_selection(query$workshop_set)
          initial_modal(FALSE)
        }
        
        if(isTRUE(query$selected_language %in% language_choices)) {
          selected_language(query$selected_language)
        } else {
          selected_language(language_initial_value)
        }
      })
      
      
      observe({
        req(!app_restored())
        req(!app_restored_from_link())
        
        mod_user_info_modal_choose(
          id = id,
          modal_dialog_title = interface_translator$t(modal_dialog_title_select_new_or_load),
          language_choices = language_choices,
          language_initial_value = language_initial_value,
          new_session_button_label = interface_translator$t(new_session_button_label),
          load_session_button_label = interface_translator$t(load_session_button_label),
          introduction_text_element = interface_translator$t(introduction_text_element)
        ) |> 
          shiny::showModal()
      })
      
      observeEvent(
        {
          req(global_selected_language())
        },
        {
          req(initial_modal()) 
          mod_user_info_modal_choose(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_select_new_or_load),
            language_choices = language_choices,
            language_initial_value = input$selected_language,
            new_session_button_label = interface_translator$t(new_session_button_label),
            load_session_button_label = interface_translator$t(load_session_button_label),
            introduction_text_element = interface_translator$t(introduction_text_element)
          ) |>
            shiny::showModal()
          
        },
        ignoreInit = TRUE
      )
      
      
      observeEvent(
        req(input$go_back),
        {
          mod_user_info_modal_choose(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_select_new_or_load),
            language_choices = language_choices,
            language_initial_value = global_selected_language(),
            new_session_button_label = interface_translator$t(new_session_button_label),
            load_session_button_label = interface_translator$t(load_session_button_label),
            introduction_text_element = interface_translator$t(introduction_text_element)
          ) |> 
            shiny::showModal()
        })
      
      # New Session from link --------------------------------------------------
      observeEvent(
        req(global_selected_language()),
        {
          req(app_restored_from_link())
          modal_user_info_modal_new_session_from_link(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_new_session),
            user_first_name_label = interface_translator$t(user_first_name_label), 
            user_last_name_label = interface_translator$t(user_last_name_label),
            submit_user_info_btn_label = interface_translator$t(submit_user_info_btn_label)
          )|> 
            shiny::showModal()
        },
        once = TRUE
      )
      
      # New Session ------------------------------------------------------------
      observeEvent(
        input$new_session, 
        {
          all_sets <- 
            load_question_sets() |> 
            dplyr::filter(question_set_active == 1) |> 
            dplyr::arrange(dplyr::desc(created))
          
          modal_user_info_modal_new_session(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_new_session),
            user_first_name_label = interface_translator$t(user_first_name_label), 
            user_last_name_label = interface_translator$t(user_last_name_label),
            workshop_selection_label = interface_translator$t(workshop_selection_label),
            workshop_selection_choices = all_sets$question_set_name,
            submit_user_info_btn_label = interface_translator$t(submit_user_info_btn_label),
            go_back_btn_label = go_back_btn_label
          )|> 
            shiny::showModal()
        }
      )
      
      # Load Session -----------------------------------------------------------
      observeEvent(
        input$load_session, 
        {
          modal_user_info_modal_load_session(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_load_session),
            load_session_text = interface_translator$t(load_session_text),
            confirm_load_session_label = interface_translator$t(confirm_load_session_label),
            go_back_btn_label = go_back_btn_label
          )|> 
            shiny::showModal()
        }
      )
      
      
      selected <- reactive(reactable::getReactableState("table", "selected"))
      
      output$table <- reactable::renderReactable({
        reactable::reactable(
          load_users_table() |> 
            dplyr::select(
              user_first_name,
              user_last_name,
              user_session,
              workshop_set,
              round_number,
              question_number,
              question_type
            ), 
          selection = "single",
          searchable = TRUE,
          theme = reactable::reactableTheme(
            backgroundColor = "#153015",
            searchInputStyle = list(color = "#153015")
          )
        )
      })
      
      ## Create an InputValidator object
      iv <- InputValidator$new()
      
      ## Add validation rules
      # iv$add_rule("user_first_name", sv_required(message = interface_translator$t(user_first_name_required_label)))
      
      iv$add_rule("user_first_name", function (value) {
        test <- function(val) {
          if (is.null(val))
            return(FALSE)
          if (inherits(val, "try-error"))
            return(FALSE)
          if (!is.atomic(val))
            return(TRUE)
          if (length(val) == 0)
            return(FALSE)
          if (all(is.na(val)))
            return(FALSE)
          if (is.character(val) && !any(nzchar(stats::na.omit(val))))
            return(FALSE)
          if (inherits(val, "shinyActionButtonValue") && val == 0)
            return(FALSE)
          TRUE
        }
        
        if (!test(value)) {
          interface_translator$t(user_first_name_required_label)
        }
      })
      
      # iv$add_rule("user_last_name", sv_required(message = interface_translator$t(user_last_name_required_label)))
      
      iv$add_rule("user_last_name", function (value) {
        test <- function(val) {
          if (is.null(val))
            return(FALSE)
          if (inherits(val, "try-error"))
            return(FALSE)
          if (!is.atomic(val))
            return(TRUE)
          if (length(val) == 0)
            return(FALSE)
          if (all(is.na(val)))
            return(FALSE)
          if (is.character(val) && !any(nzchar(stats::na.omit(val))))
            return(FALSE)
          if (inherits(val, "shinyActionButtonValue") && val == 0)
            return(FALSE)
          TRUE
        }
        
        if (!test(value)) {
          interface_translator$t(user_last_name_required_label)
        }
      })
      
      # iv$add_rule("workshop_selection", sv_required(message = interface_translator$t(workshop_selection_required_label)))
      
      iv$add_rule("workshop_selection", function (value) {
        test <- function(val) {
          if (is.null(val))
            return(FALSE)
          if (inherits(val, "try-error"))
            return(FALSE)
          if (!is.atomic(val))
            return(TRUE)
          if (length(val) == 0)
            return(FALSE)
          if (all(is.na(val)))
            return(FALSE)
          if (is.character(val) && !any(nzchar(stats::na.omit(val))))
            return(FALSE)
          if (inherits(val, "shinyActionButtonValue") && val == 0)
            return(FALSE)
          TRUE
        }
        
        if (!test(value)) {
          interface_translator$t(workshop_selection_required_label)
        }
      })
      
      
      # Submit User Info -------------------------------------------------------
      ## When the "Submit" button is clicked in the user info pop-up modal...
      
      user_first_name <- reactiveVal()
      user_last_name <- reactiveVal()
      user_session <- reactiveVal()
      workshop_selection <- reactiveVal()
      
      shiny::observeEvent(input$submit_user_info_btn, {
        
        ## Start displaying errors in the UI
        iv$enable()
        
        ## require that the "First Name" and "Last Name" fields have been populated
        shiny::req(
          input$user_first_name, 
          input$user_last_name,
          input$workshop_selection
        )
        
        user_first_name(input$user_first_name)
        user_last_name(input$user_last_name)
        user_session(session$token)
        workshop_selection(input$workshop_selection)
        
        upsert_user_info(
          pool = pool,
          user_first_name = input$user_first_name,
          user_last_name = input$user_last_name,
          user_session = session$token,
          workshop_set = input$workshop_selection,
          round_number = rctv$current_group_number,
          question_number = rctv$current_question_number,
          question_type = ""
        )
        
        initial_modal(FALSE)
        ## remove the open modal dialogue
        shiny::removeModal()
        
      })
      
      shiny::observeEvent(input$submit_user_info_btn_from_link, {
        
        ## Start displaying errors in the UI
        iv$enable()
        
        ## require that the "First Name" and "Last Name" fields have been populated
        shiny::req(
          input$user_first_name, 
          input$user_last_name
        )
        
        user_first_name(input$user_first_name)
        user_last_name(input$user_last_name)
        user_session(session$token)
        
        upsert_user_info(
          pool = pool,
          user_first_name = input$user_first_name,
          user_last_name = input$user_last_name,
          user_session = session$token,
          workshop_set = workshop_selection(),
          round_number = rctv$current_group_number,
          question_number = rctv$current_question_number,
          question_type = ""
        )
        
        initial_modal(FALSE)
        ## remove the open modal dialogue
        shiny::removeModal()
        
      })
      
      shiny::observeEvent(input$confirm_selection_btn, {
        
        selected_user <- load_users_table() |> 
          dplyr::slice(selected())
        
        
        user_first_name(selected_user$user_first_name)
        user_last_name(selected_user$user_last_name)
        user_session(selected_user$user_session)
        workshop_selection(selected_user$workshop_set)
        
        binary_responses <- load_users_binary_responses(
          pool = pool, 
          first_name = selected_user$user_first_name,
          last_name = selected_user$user_last_name,
          session = selected_user$user_session,
          workshop = selected_user$workshop_set
        ) |> 
          dplyr::select(
            Group = round_number,
            Question = question_number,
            QuestionText = question_text,
            Index = index_in_set,
            Response = response,
            Confidence = confidence,
            Truth = truth,
            Brier = brier_score
          ) |> 
          dplyr::mutate(
            Source = ""
          )
        
        rctv$binary_tbl <- rctv$binary_tbl |>
          rbind(binary_responses)
        
        range_responses <- load_users_range_responses(
          pool = pool, 
          first_name = selected_user$user_first_name,
          last_name = selected_user$user_last_name,
          session = selected_user$user_session,
          workshop = selected_user$workshop_set
        ) |> 
          dplyr::select(
            Group = round_number,
            Question = question_number,
            QuestionText = question_text,
            Index = index_in_set,
            Lower90 = lower_90,
            Upper90 = upper_90,
            Truth = truth,
            RelativeError = relative_error
          ) |> 
          dplyr::mutate(
            Source = ""
          )
        
        rctv$range_tbl <- rctv$range_tbl |>
          rbind(range_responses)
        
        rctv$current_group_number <- selected_user$round_number
        rctv$current_question_number <- selected_user$question_number
        # rctv$current_question_type <- selected_user$question_type
        
        initial_modal(FALSE)
        ## remove the open modal dialogue
        shiny::removeModal()
        
      })
      
      list(
        user_first_name = user_first_name,
        user_last_name = user_last_name,
        user_session = user_session,
        workshop_selection = workshop_selection,
        selected_language = reactive({selected_language()})
      )
    }
  )
}
