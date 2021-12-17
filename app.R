


# 1.0 SETUP ----

## 1.1 Load Packages ----
library(shiny)
library(shinyWidgets)
library(bslib)  # Bootstrap formatting 
library(glue)
library(waiter)
library(reactable)


## 1.2 Build UI Theme ----
# Develop the Bootstrap theme for the app
app_theme <- bslib::bs_theme(
  version = 5, 
  bootswatch = "sketchy", 
  bg = "#153015", 
  fg = "#FFFFFF", 
  primary = "#004F9E",   # Bonn blue
  secondary = "#FBBA00"   # Bonn yellow
)


# 2.0 UI ----
ui <- shiny::navbarPage(
  
  ## 2.1 Set Up Global UI Elements ----
  title = "Calibrator", 
  
  theme = app_theme, 
  
  collapsible = TRUE, 
  
  # Ensure tickmark text on "Confidence" sliders is white  
  shiny::tags$style(
    ".irs-grid-text {color: #FFFFFF}"   
  ), 
  
  ## 2.2 "Questions" Page ----
  shiny::tabPanel(
    title = "Questions", 
    
    # Enable use of {waiter} package 
    waiter::use_waiter(), 
    
    # Set the color for all 'inputSlider()' widgets to "Bonn Yellow"
    shinyWidgets::chooseSliderSkin(
      skin = "Shiny", 
      color = "#FBBA00"
    ), 
    
    shiny::fluidRow(
      
      ### 2.3 Questions UI Elements ----
      shiny::column(
        width = 6, 
        shiny::wellPanel(
          style = "background: #153015;", 
          
          #### 2.3.1 Question Number Header ----
          shiny::h3(
            shiny::textOutput(outputId = "question_no_ui")
          ), 
          
          shiny::hr(), 
          
          #### 2.3.2 Question Text ----
          shiny::h4(
            shiny::textOutput(outputId = "question_text_ui")
          ), 
          
          shiny::br(), 
          
          #### 2.3.3 Answer UI Elements ----
          shiny::div(
            style = "padding-left: 50px; padding-right: 50px;",
            shiny::uiOutput(outputId = "answer_ui") 
          ), 
          
          shiny::hr(), 
          
          #### 2.3.4 Previous / Next Buttons ----
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
      
      ### 2.4 Response UI Elements ----
      shiny::column(
        width = 6, 
        
        shiny::tabsetPanel(
          
          #### 2.4.1 "Binary" Results Table ----
          shiny::tabPanel(
            title = "Binary Results", 
            reactable::reactableOutput(outputId = "results_binary_tbl")
          ), 
          
          #### 2.4.1 "Range" Results Table ----
          shiny::tabPanel(
            title = "Range Results", 
            reactable::reactableOutput(outputId = "results_range_tbl")
          )
        )
        
      )
      
    )
    
  )
  
)



# 3.0 SERVER ----
server <- function(input, output, session) {
  
  ## 3.1 Build Waiting Screen ----
  w <- waiter::Waiter$new(
    id = c("answer_ui"), 
    html = shiny::tagList(
      waiter::spin_flower(), 
      "Loading Next Question..."
    ), 
    color = "#153015",
  )
  
  ## 3.2 Initialize ReactiveValues ----
  # Create a `reactiveValues` object that holds a reactive variable called 
  # 'current_question_number', which is set to 1 (to start)
  rctv <- shiny::reactiveValues(
    current_question_number = 1, 
    current_question_type = question_type_df$QuestionType[1], 
    
    binary_tbl = data.frame(
      Question = as.integer(), 
      Response = as.character(), 
      Confidence = as.character(), 
      Truth = as.character(), 
      Brier = as.numeric(), 
      stringsAsFactors = FALSE
    ), 
    
    range_tbl = data.frame(
      Question = as.integer(), 
      Lower90 = as.numeric(), 
      Upper90 = as.numeric(), 
      Truth = as.numeric(), 
      RelativeError = as.numeric()
    )
  )
  
  # 3.3 User Info Modal ----
  shiny::modalDialog(
    title = "Enter User Information", 
    shiny::tagList(
      shiny::div(
        shiny::textInput(
          inputId = "user_first_name", 
          label = "First Name", 
          placeholder = "First Name"
        ), 
        shiny::textInput(
          inputId = "user_last_name", 
          label = "Last Name", 
          placeholder = "Last Name"
        )
      )
    ), 
    easyClose = FALSE, 
    footer = shiny::tagList(
      shiny::div(
        # Button to submit user's information
        shiny::actionButton(
          inputId = "submit_user_info_btn", 
          label = "Submit", 
          icon = shiny::icon("check")
        )
      )
    )
  ) |> 
    shiny::showModal()
  
  # 3.4 "Submit" User Info Button ----
  shiny::observeEvent(input$submit_user_info_btn, {
    
    # Require that the "First Name" and "Last Name" fields have been completed
    shiny::req(input$user_first_name, input$user_last_name)
    
    # Remove the open modal dialogue
    shiny::removeModal()
    
  })
  
  
  output$tmp <- shiny::renderText(
    paste0(input$user_first_name, " ", input$user_last_name)
  )
  
  
  # 3.3 "Next" Action Button ----
  # When the "Next" button is clicked...
  shiny::observeEvent(input$next_btn, {
    
    # Capture the current response / Lower90
    rctv$current_response_1 <- eval(
      parse(text = glue::glue("input$answer_ui_{rctv$current_question_number}_A"))
    )
    
    # Capture the current Confidence / Upper90 
    rctv$current_response_2 <- eval(
      parse(text = glue::glue("input$answer_ui_{rctv$current_question_number}_B"))
    )
    
    # Create the first modal text segment
    modal_text_1 <- ifelse(
      rctv$current_question_type == "Binary", 
      "You answered:", 
      "You answered (Lower 90%):"
    )
    
    # Create the second modal text segment
    modal_text_2 <- ifelse(
      rctv$current_question_type == "Binary", 
      "With confidence:", 
      "You answered (Upper 90%):"
    )
    
    # Create the modal text suffix
    modal_text_3 <- ifelse(
      rctv$current_question_type == "Binary", 
      "%", 
      ""
    )
    
    # Launch a modal dialogue asking user to confirm their answer
    shiny::modalDialog(
      title = "Are You Sure?", 
      glue::glue("{modal_text_1} {rctv$current_response_1}"), 
      
      shiny::br(), 
      glue::glue("{modal_text_2} {rctv$current_response_2}{modal_text_3}"), 
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
      )
    ) |> 
      shiny::showModal()
    
  })
  
  # When the "Submit" button is clicked...
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
    
    if (rctv$current_question_type == "Binary") {
      
      confidence_numeric <- eval(
        parse(
          text = gsub("%", "", rctv$current_response_2)
        )
      ) / 100
      
      rctv$binary_tbl <- rctv$binary_tbl |>
        rbind(
          data.frame(
            Question = rctv$current_question_number, 
            Response = rctv$current_response_1, 
            Confidence = paste0(rctv$current_response_2, "%"), 
            Truth = questions$Binary$Answer[rctv$current_question_number], 
            Brier = brier(
              response = rctv$current_response_1, 
              confidence = confidence_numeric, 
              correct_answer = questions$Binary$Answer[rctv$current_question_number]
            ), 
            stringsAsFactors = FALSE
          )
        )
      
    } else {
      
      rctv$range_tbl <- rctv$range_tbl |>
        rbind(
          data.frame(
            Question = rctv$current_question_number, 
            Lower90 = rctv$current_response_1, 
            Upper90 = rctv$current_response_2, 
            Truth = questions$Range$Answer[questions$Range$QuestionNumber == rctv$current_question_number], 
            RelativeError = relative_error(
              lower_90 = rctv$current_response_1,
              upper_90 = rctv$current_response_2,
              correct_answer = questions$Range$Answer[questions$Range$QuestionNumber == rctv$current_question_number]
            )
          )
        )
      
    }
    
    # Require that you are not on the last question of the quiz
    shiny::req(rctv$current_question_number < max(question_type_df$QuestionNumber))
    
    # Increase the 'current_question_number' value by 1
    rctv$current_question_number <- rctv$current_question_number + 1
    
    # Get the corresponding question type for the next question
    rctv$current_question_type <- question_type_df$QuestionType[rctv$current_question_number]
    
  })
  
  
  
  output$question_ui <- shiny::uiOutput({
    
    shiny::req(
      rctv$current_question_type, 
      rctv$current_group_number, 
      rctv$current_question_number
    )
    
    if (rctv$current_question_type == "Binary") {
      
      binary_ui %>% 
        purrr::pluck(
          glue::glue("Group_{rctv$current_group_number}"), 
          glue::glue("question_{rctv$current_question_number}")
        )
      
    } else {
      
      binary_ui %>% 
        purrr::pluck(
          glue::glue("Group_{rctv$current_group_number}"), 
          glue::glue("question_{rctv$current_question_number}")
        )
      
    }
    
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
        shinyWidgets::awesomeRadio(
          inputId = glue::glue("answer_ui_{rctv$current_question_number}_A"),
          label = "Answer:",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE",
          status = "warning"
        ), 
        shiny::sliderInput(
          inputId = glue::glue("answer_ui_{rctv$current_question_number}_B"), 
          label = "Confidence:", 
          min = 50, 
          max = 100, 
          value = 60, 
          step = 5, 
          post = "%"
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
          )
        ), 
        
        shiny::div(
          style = "display: inline-block;",
          shiny::numericInput(
            inputId = glue::glue("answer_ui_{rctv$current_question_number}_B"),
            label = "Upper Bound",
            value = 100
          )
        )
      )
      
    }
    
  })
  
  # Create the table to hold the "Binary" results & scores
  output$results_binary_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$binary_tbl)
    
    reactable::reactable(
      rctv$binary_tbl, 
      columns = list(
        Brier = reactable::colDef(
          format = reactable::colFormat(digits = 2)
        )
      ), 
      theme = reactable::reactableTheme(
        backgroundColor = "#153015"
      )
    )
    
  })
  
  # Create the table to hold the "Range" results & scores
  output$results_range_tbl <- reactable::renderReactable({
    
    shiny::req(rctv$range_tbl)
    
    reactable::reactable(
      rctv$range_tbl, 
      columns = list(
        Lower90 = reactable::colDef(name = "Lower Bound"),
        Upper90 = reactable::colDef(name = "Upper Bound"), 
        RelativeError = reactable::colDef(
          name = "Relative Error", 
          format = reactable::colFormat(digits = 2)
        )
      ), 
      columnGroups = list(
        reactable::colGroup(
          name = "90% Confidence Interval", 
          columns = c("Lower90", "Upper90")
        )
      ), 
      theme = reactable::reactableTheme(
        backgroundColor = "#153015"
      )
    )
    
  })
  
}

shinyApp(ui, server)
