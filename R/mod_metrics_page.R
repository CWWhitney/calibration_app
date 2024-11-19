##******************************************************************************
##*
##* This is the metrics_page shiny module which description
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
#' @seealso [mod_metrics_page_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_metrics_page_ui(
#'          id = "mod_metrics_page"
#'        )
#'      ),
#'      server = function(input, output, session) {
#'        mod_metrics_page_server(
#'          id ="mod_metrics_page"
#'        )
#'      }
#'    )
#'  
#'  }
mod_metrics_page_ui <- function(id, tab_title, left_column_header, right_column_header, text_center) {
  ns <- NS(id)
  shiny::tabPanel(
    title = tab_title, 
    
    shiny::fluidRow(
      
      shiny::column(
        width = 6, 
        
        shiny::h2(left_column_header), 
        echarts4r::echarts4rOutput(outputId = "binary_metrics_chart")
      ), 
      
      shiny::column(
        width = 6, 
        
        shiny::h2(right_column_header), 
        echarts4r::echarts4rOutput(outputId = "range_metrics_chart")
      )
    ), 
    
    shiny::hr(), 
    
    shiny::fluidRow(
      shiny::h4(
        class = "text-center", 
        text_center
      )
    )
  )
}



#' @param id The namespace id of the module.
#'
#' @return `list` of `shiny::reactive({})`s
#'
#' @seealso [metrics_page_ui()]
#'
#' @export
#'
#' @inherit mod_metrics_page_ui title description details examples
mod_metrics_page_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
    }
  )
}