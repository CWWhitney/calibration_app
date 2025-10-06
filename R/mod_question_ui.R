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
    word_for_upper_bound,
    next_btn_label
) {
  ns <- NS(id)
  
  bslib::card(
    bslib::card_header(h3(word_for_question, question_row_number), class = "bg-dark"),
    
    shiny::h4(question), 
    
    if (type == "binary") {
      bslib::layout_column_wrap(
        width = 1/2,
        shinyWidgets::prettyRadioButtons(
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
      
    },
    bslib::card_footer(
      #### Previous / Next Buttons ------------------------------------------
      class = "bg-dark d-flex justify-content-end",
      shiny::actionButton(
        class = "btn btn-lg", 
        inputId = ns("next_btn"), 
        label = next_btn_label, 
        icon = shiny::icon(name = "arrow-right")
      )
    )
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
mod_question_server <- function(id, question_type, required_text_label, number_text_label, left_lower_label) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ## Main validator
      iv <- InputValidator$new()
      
      ## Conditional validator for "binary" type
      binary_iv <- InputValidator$new()
      binary_iv$condition(~ question_type() == "binary")
      
      # binary_iv$add_rule("input_A", sv_required(interface_translator$t(required_text_label)))
      # binary_iv$add_rule("input_B", sv_required(interface_translator$t(required_text_label)))
      
      
      binary_iv$add_rule("input_A", function (value) {
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
          interface_translator$t(required_text_label)
        }
      })
      
      binary_iv$add_rule("input_B", function (value) {
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
          interface_translator$t(required_text_label)
        }
      })
      
      
      
      ## Conditional validator for "range" type
      range_iv <- InputValidator$new()
      range_iv$condition(~ question_type() == "range")
      
      # range_iv$add_rule("input_A", sv_required(interface_translator$t(number_text_label)))
      # range_iv$add_rule("input_B", sv_required(interface_translator$t(number_text_label)))
      
      range_iv$add_rule("input_A", function (value) {
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
          interface_translator$t(number_text_label)
        }
      })
      
      range_iv$add_rule("input_B",  function (value) {
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
          interface_translator$t(number_text_label)
        }
      })
      
      range_iv$add_rule("input_A", function(value) {
        if (input$input_B < input$input_A && !is.na(input$input_A) && !is.na(input$input_B)) {
          interface_translator$t(left_lower_label)
        }
      })
      
      range_iv$add_rule("input_B", function(value) {
        if (input$input_B < input$input_A && !is.na(input$input_A) && !is.na(input$input_B)) {
          ""
        }
      })
      
      ## Add the conditional validator to the main validator
      iv$add_validator(binary_iv)
      iv$add_validator(range_iv)
      
      return(
        list(
          iv = iv,
          A = reactive(input$input_A),
          B = reactive(input$input_B),
          next_btn = reactive(input$next_btn)
        )
      )
    }
  )
}