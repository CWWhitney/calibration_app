
library(shiny)


ui <- shiny::navbarPage(
  
  title = "Calibrator", 
  
  shiny::tabPanel(
    title = "Questions", 
    
    shiny::verbatimTextOutput(
      outputId = "tmp"
    )
    
  )
  
)




server <- function(input, output, session) {
  
  questions <- read.csv("data/example_questions_db.csv")
  # TODO // assess whether `bindCache()` would be useful or not
  
  output$tmp <- shiny::renderPrint(
    questions$Question
  )
  
}

shinyApp(ui, server)