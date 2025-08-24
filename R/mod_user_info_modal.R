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


modal_user_info_modal_new_session <- 
  function(
    id,
    modal_dialog_title,
    user_first_name_label, 
    user_last_name_label,
    workshop_selection_label,
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
        ),
        bslib::layout_column_wrap(
          width = 1,
          shiny::selectInput(
            inputId = ns("workshop_selection"), 
            label = workshop_selection_label,
            choices = c(1,2,3)
          )
        )
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
          )
        )
      )
    )
    
  }


modal_user_info_modal_load_session <- function(
    id,
    modal_dialog_title,
    load_session_text,
    table_data,
    confirm_load_session_label
) {
  ns <- NS(id)
  shiny::modalDialog(
    title = modal_dialog_title,
    shiny::tagList(
      shiny::div(
        shiny::p(load_session_text),
        reactable::reactable(
          table_data, 
          selection = "single",
          searchable = TRUE,
          theme = reactable::reactableTheme(
            backgroundColor = "#153015",
            searchInputStyle = list(color = "#153015")
          )
        )
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
        )
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
    introduction_text_elements,
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
    question_index,
    app_restored,
    rctv
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      # App Restore ------------------------------------------------------------
      observe({
        req(!app_restored())
        
        mod_user_info_modal_choose(
          id = id,
          modal_dialog_title = interface_translator$t(modal_dialog_title_select_new_or_load),
          language_choices = language_choices,
          language_initial_value = language_initial_value,
          new_session_button_label = interface_translator$t(new_session_button_label),
          load_session_button_label = interface_translator$t(load_session_button_label),
          introduction_text_elements = interface_translator$t(introduction_text_elements)
        ) |> 
          shiny::showModal()
      })
      
      # New Session ------------------------------------------------------------
      observeEvent(
        input$new_session, 
        {
          modal_user_info_modal_new_session(
            id = id,
            modal_dialog_title = interface_translator$t(modal_dialog_title_new_session),
            user_first_name_label = interface_translator$t(user_first_name_label), 
            user_last_name_label = interface_translator$t(user_last_name_label),
            workshop_selection_label = interface_translator$t(workshop_selection_label),
            submit_user_info_btn_label = interface_translator$t(submit_user_info_btn_label)
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
            modal_dialog_title = modal_dialog_title_load_session,
            load_session_text = load_session_text,
            table_data = 
              data.frame(
                user_first_name = c("a", "b", "b"),
                user_last_name = c("a", "b", "b"),
                session_token =
                  c(
                    "bf5fe108b33dcbb3a3e3df3a0dda2717",
                    "ec5500d4e1083a66f595921a3ecae7c7",
                    "f796601e6885f8684cd34c70ee70ac37"
                  ),
                question_set = c(
                  1,1,2
                )
              ),
            confirm_load_session_label = confirm_load_session_label
          )|> 
            shiny::showModal()
        }
      )
      
      ## Create an InputValidator object
      iv <- InputValidator$new()
      
      ## Add validation rules
      iv$add_rule("user_first_name", sv_required(message = interface_translator$t(user_first_name_required_label)))
      iv$add_rule("user_last_name", sv_required(message = interface_translator$t(user_last_name_required_label)))
      iv$add_rule("workshop_selection", sv_required(message = interface_translator$t(workshop_selection_required_label)))
      
      
      # Submit User Info -------------------------------------------------------
      ## When the "Submit" button is clicked in the user info pop-up modal...
      
      shiny::observeEvent(input$submit_user_info_btn, {
        
        ## Start displaying errors in the UI
        iv$enable()
        
        ## require that the "First Name" and "Last Name" fields have been populated
        shiny::req(
          input$user_first_name, 
          input$user_last_name,
          input$workshop_selection
        )
        
        ## remove the open modal dialogue
        shiny::removeModal()
        
      })
      
      list(
        user_first_name = reactive({input$user_first_name}),
        user_last_name = reactive({input$user_last_name}),
        selected_language = reactive({input$selected_language}),
        current_group_number = reactive({rctv$current_group_number}),
        current_question_number = reactive({rctv$current_question_number}),
        current_question_type = reactive({rctv$current_question_type})
      )
    }
  )
}
