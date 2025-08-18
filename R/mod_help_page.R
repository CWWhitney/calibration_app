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
    # for text
    # look for shiny::h1 to h5 or more for header sizes
    # shiny::p() for regular text (times new roman)
    shiny::h4("More Content here..."), 
    shiny::p("text here..."),
    # shiny::br() is a break or a new line
    # shiny::hr() is a break and a line
    # across the page 'horizontal rule'
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
