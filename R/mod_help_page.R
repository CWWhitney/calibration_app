##******************************************************************************
##*
##* This is the help_page shiny module which description
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
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_help_page_ui(
#'          id = "mod_help_page"
#'        )
#'      ),
#'      server = function(input, output, session) {}
#'    )
#'  
#'  }
mod_help_page_ui <- function(id, tab_title, url = "'https://www.youtube.com/embed/7P2YI9-smfU'") {
  ns <- NS(id)
  shiny::tabPanel(
    title = tab_title, 
    shiny::h4("More Content here..."), 
    shiny::p("text here..."),
    shiny::HTML(
      glue::glue(
        "<iframe width='560' height='315'", 
        "src={url}", 
        "title='YouTube video player' frameborder='0' allow='accelerometer;", 
        "autoplay; clipboard-write; encrypted-media; gyroscope;", 
        "picture-in-picture' allowfullscreen></iframe>", 
        .sep = " "
      )
    )
  )
}
