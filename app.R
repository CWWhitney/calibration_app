
library(shiny)
library(bslib)

# Load data outside of server, so it can be shared across all user sessions
questions <- read.csv("data/example_questions_db.csv")

app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  bg = "#153015", 
  fg = "#FFFFFF", 
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
          
          shiny::h2(
            shiny::textOutput(outputId = "question_no_ui")
          ), 
          
          shiny::hr(), 
          
          shiny::h4(
            shiny::textOutput(outputId = "question_text_ui")
          ), 
          
          shiny::br(), 
          
          shiny::uiOutput(outputId = "answer_ui"), 
          
          shiny::hr(), 
          
          # 1.3 Previous / Next Buttons ----
          # Create a button to go back to the prior question
          shiny::div(
            style = "float: right;",
            shiny::actionButton(
              class = "btn btn-lg", 
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
  
  # rctv$answers_range <- data.frame()
  # rctv$answers_binary <- data.frame()
  
  # When the "Next" button is clicked...
  shiny::observeEvent(input$next_btn, {
    
    current_answer_id <- eval(
      parse(text = paste0("input$answer_ui_", rctv$current_question_no, "_A"))
    )
    
    # Launch a modal dialogue asking user to confirm their answer
    shiny::modalDialog(
      title = "Are You Sure?", 
      glue::glue("You answered: {current_answer_id}"), 
      easyClose = FALSE, 
      footer = shiny::tagList(
        shiny::div(
          # Button to dismiss the modal
          shiny::modalButton(
            label = "Go Back", 
            icon = shiny::icon("pen")
          ), 
          # Button to move to the next question
          shiny::actionButton(
            inputId = "submit_answer_btn", 
            label = "Submit", 
            icon = shiny::icon("check")
          )
        )
      ),
    ) |> 
      shiny::showModal()
    
  })
  
  # When the "Next" button is clicked...
  shiny::observeEvent(input$submit_answer_btn, {

    # ... remove the open modal dialogue
    shiny::removeModal()
    
    # Require that you are not on the last question of the quiz
    shiny::req(rctv$current_question_no < nrow(questions))

    # Increase the 'current_question_no' value by 1
    rctv$current_question_no <- rctv$current_question_no + 1

  })

  # Create the Question UI Header
  output$question_no_ui <- shiny::renderText(
    
    paste0("Question ", rctv$current_question_no, ":")
    
  )
  
  # Create the Question UI question text
  output$question_text_ui <- shiny::renderText(
    
    questions$Question[rctv$current_question_no]
    
  )
  
  # Create the Answer UI
  output$answer_ui <- shiny::renderUI({
    
    if (questions$Answer[rctv$current_question_no] %in% c("T", "F")) {
      
      shiny::tagList(
        shiny::radioButtons(
          inputId = paste0("answer_ui_", rctv$current_question_no, "_A"), 
          label = "Answer:", 
          choices = c("TRUE", "FALSE")
        ), 
        shiny::selectInput(
          inputId = paste0("answer_ui_", rctv$current_question_no, "_B"), 
          label = "Confidence:", 
          choices = paste0(seq.int(from = 50, to = 100, by = 10), "%")
        )
      )
      
    } else {
      
      shiny::tagList(
        shiny::h5("90% Confidence Interval:"), 
        shiny::div(
          style = "display: inline-block;", 
          shiny::numericInput(
            inputId = paste0("answer_ui_", rctv$current_question_no, "_A"), 
            label = "Lower Bound", 
            value = 0
          ), 
          shiny::numericInput(
            inputId = paste0("answer_ui_", rctv$current_question_no, "_B"), 
            label = "Upper Bound", 
            value = 100
          )
        )
      )
      
    }
    
  })
    
}

shinyApp(ui, server)