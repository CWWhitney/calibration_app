##******************************************************************************
##*
##* This is the question shiny module which can be called to display a question.
##*
##******************************************************************************

#' Question Module
#'
#' A module, which renders a question interface and returns the selected answer.
#'
#' @param id The namespace id of the module.
#'
#' @return An HTML element for use in a UI.
#'
#'
#' @seealso [mod_question_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_question_ui(
#'          id = "mod_question"
#'        )
#'      ),
#'      server = function(input, output, session) {
#'        mod_question_server(
#'          id ="mod_question"
#'        )
#'      }
#'    )
#'  
#'  }
mod_question_ui <- function(
    id, 
    question,
    question_row_number,
    type,
    word_for_question,
    word_for_answer,
    word_for_correct,
    word_for_incorrect,
    word_for_confidence,
    word_for_confidence_interval,
    word_for_lower_bound,
    word_for_upper_bound
) {
  ns <- NS(id)
  
  shiny::tagList(
    
    shiny::h3(
      word_for_question, question_row_number
    ), 
    
    shiny::hr(), 
    
    shiny::h4(question), 
    
    shiny::br(), 
    
    if (type == "binary") {
      bslib::layout_column_wrap(
        width = 1/2,
        shinyWidgets::awesomeRadio(
          inputId = ns("input_A"),
          label = word_for_answer,
          choices = list(TRUE, FALSE) |> purrr::set_names(c(word_for_correct, word_for_incorrect)),
          selected = NA,
          status = "warning"
        ),
        shiny::sliderInput(
          inputId = ns("input_B"), 
          label = word_for_confidence, 
          min = 50, 
          max = 100, 
          value = 50, 
          step = 5, 
          post = "%"
        )
      )
      
    } else {
      
      shiny::tagList(
        shiny::h5(word_for_confidence_interval),
        bslib::layout_column_wrap(
          width = 1/2,
          shinysurveys::numberInput(
            inputId = ns("input_A"),
            label = word_for_lower_bound,
            placeholder = 0
          ),
          shinysurveys::numberInput(
            inputId = ns("input_B"),
            label = word_for_upper_bound,
            placeholder = 0
          )
        )
      )
      
    }
    
  )
}



#' @param id The namespace id of the module.
#'
#' @return `list` of `shiny::reactive({})`s
#'
#' @seealso [question_ui()]
#'
#' @export
#'
#' @inherit mod_question_ui title description details examples
mod_question_server <- function(id, question_type, required_text_label, left_lower_label) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ## Create an InputValidator object
      iv <- InputValidator$new()
      
      ## Add validation rules
      iv$add_rule("input_A", sv_required(interface_translator$t(required_text_label)))
      iv$add_rule("input_B", sv_required(interface_translator$t(required_text_label)))
      
      if(question_type == "range") {
        iv$add_rule("input_A", function(value) {
          if (input$input_B < input$input_A & !is.na(input$input_A) & !is.na(input$input_B)) {
            interface_translator$t(left_lower_label)
          }
        })
        
        iv$add_rule("input_B", function(value) {
          if (input$input_B < input$input_A & !is.na(input$input_A) & !is.na(input$input_B)) {
            ""
          }
        })
      }
      
      ## Start displaying errors in the UI
      iv$enable()
      
      
      return(
        list(
          is_valid = reactive(iv$is_valid()),
          A = reactive(input$input_A),
          B = reactive(input$input_B)
        )
      )
    }
  )
}