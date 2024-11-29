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
mod_user_info_modal_ui <- 
  function(
    id,
    modal_dialog_title,
    user_first_name_label, 
    user_last_name_label,
    submit_user_info_btn_label
    ) {
  ns <- NS(id)
  tagList(
    shiny::modalDialog(
      title = modal_dialog_title,
      shiny::tagList(
        shiny::div(
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
      footer = shiny::tagList(
        shiny::div(
          # Button to submit user's information
          shiny::actionButton(
            inputId = ns("submit_user_info_btn"), 
            label = submit_user_info_btn_label,
            icon = shiny::icon("check")
          )
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
    modal_dialog_title,
    user_first_name_label, 
    user_last_name_label,
    submit_user_info_btn_label,
    question_index,
    rctv
    ) {
  moduleServer(
    id,
    function(input, output, session) {
       ns <- session$ns
       
       mod_user_info_modal_ui(
         id = id,
         modal_dialog_title = modal_dialog_title,
         user_first_name_label = user_first_name_label, 
         user_last_name_label = user_last_name_label,
         submit_user_info_btn_label = submit_user_info_btn_label
         )|> 
         shiny::showModal()
       
       ## 3.3 "Submit User Info" Button ----
       # When the "Submit" button is clicked in the user info pop-up modal...
       shiny::observeEvent(input$submit_user_info_btn, {
         
         # require that the "First Name" and "Last Name" fields have been populated
         shiny::req(
           input$user_first_name, 
           input$user_last_name
         )
         
         # remove the open modal dialogue
         shiny::removeModal()
         
         # 
         # # list any existing {pins} history for this user
         # user_history <-
         #   pins::pin_search(
         #     board = board,
         #     search = glue::glue("{input$user_last_name}_{input$user_first_name}")
         #   )
         # 
         # # if the user has completed a group (that wrote successfully to a pin)...
         # if (nrow(user_history) > 0) {
         #   
         #   # ... get the highest group number that was completed
         #   last_group_completed <- user_history %>% 
         #     split(.$name) %>% 
         #     purrr::map_dfr(
         #       function(x) pins::pin_read(board = board, name = x$name) |> dplyr::select(Group), 
         #       .id = "source"
         #     ) %>% 
         #     dplyr::pull(Group) |> 
         #     max()
         #   
         #   # set the current group number to the *next* group
         #   rctv$current_group_number <- last_group_completed + 1
         #   
         #   # retrieve the corresponding current question type
         #   rctv$current_question_type <- question_index$QuestionType[question_index$Group == rctv$current_group_number][1]
         #   
         #   # lookup the first question (index) for the *next* group
         #   rctv$current_question_number <- question_index$Index[question_index$Group == rctv$current_group_number & question_index$QuestionNumber == 1 & question_index$QuestionType == rctv$current_question_type]
         #   
         #   # update the reactive 'binary_tbl' and 'range_tbl' with the user's history
         #   binary_history <- pins::pin_read(
         #     board = board, 
         #     name = glue::glue("binary_{input$user_last_name}_{input$user_first_name}")
         #   )
         #   
         #   range_history <- pins::pin_read(
         #     board = board, 
         #     name = glue::glue("range_{input$user_last_name}_{input$user_first_name}")
         #   )
         #   
         #   rctv$binary_tbl <- rctv$binary_tbl |> 
         #     dplyr::bind_rows(
         #       binary_history |> dplyr::select(-User)
         #     )
         #   
         #   rctv$range_tbl <- rctv$range_tbl |> 
         #     dplyr::bind_rows(
         #       range_history |> dplyr::select(-User)
         #     )
         #   
         # } else {
           
           # ... otherwise, if no user history (pin) was found, start at the beginning
           rctv$current_group_number = 1
           
           rctv$current_question_number = 1
           
           rctv$current_question_type <- question_index$QuestionType[1]
           
         # }
         
       })
       
       list(
         user_first_name = reactive({input$user_first_name}),
         user_last_name = reactive({input$user_last_name}),
         current_group_number = reactive({rctv$current_group_number}),
         current_question_number = reactive({rctv$current_question_number}),
         current_question_type = reactive({rctv$current_question_type})
       )
    }
  )
}