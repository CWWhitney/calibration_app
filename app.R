
library(shiny)
library(bslib)

# Load data outside of server, so it can be shared across all user sessions
questions <- read.csv("data/example_questions_db.csv")

app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00"   # Bonn yellow
)

ui <- shiny::navbarPage(
  
  title = "Calibrator", 
  
  theme = app_theme, 
  
  collapsible = TRUE, 
  
  shiny::tabPanel(
    title = "Questions", 
    
    shiny::fluidRow(
      
      # Column for the assessment questions
      shiny::column(
        width = 7, 
        shiny::wellPanel(
          
          shiny::h2("Question 1:"), 
          
          shiny::hr(), 
          
          shiny::p("The question ..."), 
          
          shiny::br(), 
          
          shiny::p("Placeholder for response options ..."), 
          
          shiny::hr(), 
          
          # 1.3 Previous / Next Buttons ----
          # Create a button to go back to the prior question 
          shiny::div(
            
            shiny::actionButton(
              class = "btn btn-secondary", 
              inputId = "back_btn",
              label = "Previous",
              icon = shiny::icon(name = "arrow-left")
            ),
            
            # Create a button to go to the next question
            shiny::div(
              style = "float:right;", 
              shiny::actionButton(
                class = "btn btn-primary", 
                inputId = "next_btn", 
                label = "Next", 
                icon = shiny::icon(name = "arrow-right")
              )
            )
            
          )
          
        )
        
      ), 
      
      # Column for results 
      shiny::column(
        width = 5, 
        
        shiny::h4("We'll make a reactive table containing the assessment results here")
        
      )
      
    )
    
  )
  
)
  
  
  
  
  server <- function(input, output, session) {
    
    # output$tmp <- shiny::renderPrint(
    #   questions$Question
    # )
    
  }
  
  shinyApp(ui, server)