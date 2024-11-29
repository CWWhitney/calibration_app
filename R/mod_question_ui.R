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
    type
    ) {
  ns <- NS(id)

  shiny::tagList(
    
    shiny::h3(
      paste0(selected_language[44], question_row_number)
    ), 
    
    shiny::hr(), 
    
    shiny::h4(question), 
    
    shiny::br(), 
    
    if (type == "binary") {
      
      shiny::tagList(
        shiny::div(
          style = "padding-left: 20px;", 
          shinyWidgets::awesomeRadio(
            inputId = ns("input_A"),
            label = selected_language[45],
            choices = c(selected_language[31], selected_language[32]),
            selected = selected_language[31],
            status = "warning"
          )
        ), 
        
        shiny::sliderInput(
          inputId = ns("input_B"), 
          label = selected_language[46], 
          min = 50, 
          max = 100, 
          value = 60, 
          step = 5, 
          post = "%"
        )
      )
      
    } else {
      
      shiny::tagList(
        shiny::h5(selected_language[35]),
        
        shiny::div(
          style = "display: inline-block;",
          shiny::numericInput(
            inputId = ns("input_A"),
            label = selected_language[33],
            value = 0
          )
        ), 
        
        shiny::div(
          style = "display: inline-block;",
          shiny::numericInput(
            inputId = ns("input_B"),
            label = selected_language[34],
            value = 0
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
mod_question_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
       ns <- session$ns
       
       
       return(
         list(
           A = reactive(input$input_A),
           B = reactive(input$input_B)
         )
       )
    }
  )
}