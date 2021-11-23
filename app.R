
library(shiny)
library(bslib)
# library(shinyjs)

# Load data outside of server, so it can be shared across all user sessions
questions <- read.csv("data/example_questions_db.csv")

app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  bg = "#929084", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00"   # Bonn yellow
)

ui <- shiny::navbarPage(
  
  # shinyjs::useShinyjs(), 
  
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
          
          shiny::h2(
            shiny::textOutput(outputId = "question_no_ui")
          ), 
          
          shiny::hr(), 
          
          shiny::h4(
            shiny::textOutput(outputId = "question_text_ui")
          ), 
          
          shiny::br(), 
          
          shiny::p("Placeholder for response options ..."), 
          
          shiny::hr(), 
          
          # 1.3 Previous / Next Buttons ----
          # Create a button to go back to the prior question
          shiny::div(
            style = "display:inline-block; float: right;",
            shiny::actionButton(
              inputId = "next_btn", 
              label = "Next", 
              icon = shiny::icon(name = "arrow-right")
            )
          ), 
          
          # Add some extra padding below the button
          shiny::br(), 
          shiny::br()
          
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
  
  # Create a `reactiveValues` object that holds a reactive variable called 
  # 'current_question_no', which is set to 1 (to start)
  rctv <- shiny::reactiveValues(current_question_no = 1)
  
  # When the "Next" button is clicked...
  shiny::observeEvent(input$next_btn, {
    
    # ... require that you are not on the last question of the quiz
    shiny::req(rctv$current_question_no < nrow(questions))
    
    # Increase the 'current_question_no' value by 1
    rctv$current_question_no <- rctv$current_question_no + 1
    
  })
  
  # When the "Next" button is clicked...
  shiny::observeEvent(input$back_btn, {
    
    # ... require that you are not on the first question of the quiz
    shiny::req(rctv$current_question_no > 1)
    
    # Decrease the 'current_question_no' value by 1
    rctv$current_question_no <- rctv$current_question_no - 1
    
  })
  
  # Trying to disable "Back"/"Next" buttons when it makes sense to do so
  # shiny::observe({
  # 
  #   if (rctv$current_question_no == 1) {
  # 
  #     shinyjs::disable(id = "back_btn")
  # 
  #   } else {}
  # 
  # })
  
  # Create the Question UI Header
  output$question_no_ui <- shiny::renderText(
    
    paste0("Question ", rctv$current_question_no, ":")
    
  )
  
  # Create the Question UI question text
  output$question_text_ui <- shiny::renderText(
    
    questions$Question[rctv$current_question_no]
    
  )
  
}

shinyApp(ui, server)