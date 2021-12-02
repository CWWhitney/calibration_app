
library(shiny)
library(bslib)
library(glue)
library(waiter)

# Load data outside of server, so it can be shared across all user sessions
questions <- list(
  Binary = read.csv("data/questions/example_questions_db_binary.csv"), 
  Range = read.csv("data/questions/example_questions_db_range.csv")
)

# Subset the data frames in 'questions' to keep only the "QuestionNumber" and 
# "QuestionType" columns
question_type_df <- lapply(
  questions, 
  function(x) x[, c("QuestionNumber", "QuestionType")]
)

# Combine the listed data frames in 'question_type_df' into a single data frame
question_type_df <- do.call("rbind", question_type_df)

# Stop execution if there are any redundant or missing question numbers
if (length(unique(question_type_df$QuestionNumber)) != nrow(question_type_df)) {
  
  stop("Redundant \"QuestionNumber\ found; please correct and try again.")
  
}

if (max(question_type_df$QuestionNumber) != nrow(question_type_df)) {
  
  stop("A \"QuestionNumber\ was missed; please correct and try again.")
  
}

# Develop the Bootstrap theme for the app
app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  bg = "#153015", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00"   # Bonn yellow
)

# Build the UI ----
ui <- shiny::navbarPage(
  
  title = "Calibrator", 
  
  theme = app_theme, 
  
  collapsible = TRUE, 
  
  shiny::tabPanel(
    title = "Questions", 
    
    waiter::use_waiter(), 
    
    shiny::fluidRow(
      
      # Column for the assessment questions
      shiny::column(
        width = 7, 
        shiny::wellPanel(
          style = "background: #153015;", 
          
          shiny::h2(
            shiny::textOutput(outputId = "question_no_ui")
          ), 
          
          shiny::hr(), 
          
          shiny::h4(
            shiny::textOutput(outputId = "question_text_ui")
          ), 
          
          shiny::br(), 
          
          shiny::div(
            style = "padding-left: 50px; padding-right: 50px;",
            shiny::uiOutput(outputId = "answer_ui") 
          ), 
          
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
  
  w <- waiter::Waiter$new(
    id = c("answer_ui"),
    html = shiny::tagList(
      waiter::spin_flower(), 
      "Loading Next Question..."
    ), 
    color = "#153015",
  )
  
  # Create a `reactiveValues` object that holds a reactive variable called 
  # 'current_question_number', which is set to 1 (to start)
  rctv <- shiny::reactiveValues(
    current_question_number = 1, 
    current_question_type = question_type_df$QuestionType[1]
  )
  
  # When the "Next" button is clicked...
  shiny::observeEvent(input$next_btn, {
    
    rctv$current_response_1 <- eval(
      parse(text = glue::glue("input$answer_ui_{rctv$current_question_number}_A"))
    )
    
    rctv$current_response_2 <- eval(
      parse(text = glue::glue("input$answer_ui_{rctv$current_question_number}_B"))
    )
    
    modal_text_1 <- ifelse(
      rctv$current_question_type == "Binary", 
      "You answered:", 
      "You answered (Lower 90%):"
    )
    
    modal_text_2 <- ifelse(
      rctv$current_question_type == "Binary", 
      "With confidence:", 
      "You answered (Upper 90%):"
    )
    
    # Launch a modal dialogue asking user to confirm their answer
    shiny::modalDialog(
      title = "Are You Sure?", 
      glue::glue("{modal_text_1} {rctv$current_response_1}"), 
      shiny::br(), 
      glue::glue("{modal_text_2} {rctv$current_response_2}"), 
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
    
    # Show the waiting screen
    w$show()
    
    # Write out the current response to the database
    write_to_db(
      question_type = rctv$current_question_type, 
      user = Sys.getenv("USERNAME"), 
      question_number = rctv$current_question_number, 
      answer_1 = rctv$current_response_1, 
      answer_2 = rctv$current_response_2
    )
    
    # Require that you are not on the last question of the quiz
    shiny::req(rctv$current_question_number < max(question_type_df$QuestionNumber))

    # Increase the 'current_question_number' value by 1
    rctv$current_question_number <- rctv$current_question_number + 1
    
    # Get the corresponding question type for the next question
    rctv$current_question_type <- question_type_df$QuestionType[rctv$current_question_number]

  })

  # Create the Question UI Header
  output$question_no_ui <- shiny::renderText(
    
    glue::glue("Question {rctv$current_question_number}:")
    
  )
  
  # Create the Question UI question text
  output$question_text_ui <- shiny::renderText({
    
    eval(
      parse(text = glue::glue(
        "questions${rctv$current_question_type}$Question", 
        "[questions${rctv$current_question_type}$QuestionNumber == rctv$current_question_number]"
      ))
    )
    
  })
  
  # Create the Answer UI
  output$answer_ui <- shiny::renderUI({

    if (rctv$current_question_type == "Binary") {

      shiny::tagList(
        shiny::radioButtons(
          inputId = glue::glue("answer_ui_{rctv$current_question_number}_A"),
          label = "Answer:",
          choices = c("TRUE", "FALSE")
        ),
        shiny::selectInput(
          inputId = glue::glue("answer_ui_{rctv$current_question_number}_B"),
          label = "Confidence:",
          choices = paste0(seq.int(from = 50, to = 100, by = 10), "%"),
          width = "50%"
        )
      )

    } else {

      shiny::tagList(
        shiny::h5("90% Confidence Interval:"),
        shiny::div(
          style = "display: inline-block;",
          shiny::numericInput(
            inputId = glue::glue("answer_ui_{rctv$current_question_number}_A"),
            label = "Lower Bound",
            value = 0
          ),
          shiny::numericInput(
            inputId = glue::glue("answer_ui_{rctv$current_question_number}_B"),
            label = "Upper Bound",
            value = 100
          )
        )
      )

    }

  })
    
}

shinyApp(ui, server)