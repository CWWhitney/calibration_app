### CALIBRATION APP
### UNIVERSITY OF BONN
### DEVELOPED BY: KETCHBROOK ANALYTICS (MTHOMAS@KETCHBROOKANALYTICS.COM)


# 1.0 SETUP ----

## 1.1 Load Packages ----
library(shiny)
library(shinyWidgets)   # formatting sliderInput() widgets
library(bslib)  # Bootstrap formatting 
library(glue)   # convenient string pasting
library(waiter)   # loading screens
library(reactable)   # interactive tables
library(dplyr)   # general data prep
library(purrr)   # working with lists
library(tidyr)   # more data prep
library(echarts4r)   # interactive charts
library(pins)
library(googlesheets4)
library(fs)


## 1.1 Setup Environment ----
# Run "global.R" script to load shared objects across all sessions
source("global.R")

# Create a data frame that indexes all of the workshop questions
question_index <- questions %>% 
  purrr::map_dfr(
    ~ dplyr::select(.x, Group, QuestionNumber), 
    .id = "QuestionType"
  ) %>% 
  dplyr::mutate(Group = as.integer(stringr::str_sub(Group, -1, -1))) %>% 
  dplyr::arrange(Group, QuestionType, QuestionNumber) %>% 
  dplyr::mutate(Index = dplyr::row_number())


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
  title = "Kalibrator",
  
  theme = app_theme,
  
  collapsible = TRUE, 
  
  # Ensure tickmark text on "Konfidenz" sliders is white  
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet", 
      type = "text/css", 
      href = "styling.css"
    )  
  ), 
  
  ## 2.2 "Questions" Page ----
  shiny::tabPanel(
    title = "Fragen", 
    
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
          
          #### 2.3.1 Question & Response UI ----
          shiny::uiOutput(outputId = "question_ui"), 
          
          shiny::hr(), 
          
          #### 2.3.4 Previous / Next Buttons ----
          # Create a button to go back to the prior question
          shiny::div(
            style = "float: right;",
            shiny::actionButton(
              class = "btn btn-lg", 
              inputId = "next_btn", 
              label = "Weiter", 
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
        
        # shiny::verbatimTextOutput("tmp"),
        
        shiny::tabsetPanel(
          id = "results_tabset", 
          
          #### 2.4.1 "Binary" Results Table ----
          shiny::tabPanel(
            title = "Ergebnisse Binär", 
            value = "binary_results_panel", 
            reactable::reactableOutput(outputId = "results_binary_tbl")
          ), 
          
          #### 2.4.1 "Range" Results Table ----
          shiny::tabPanel(
            title = "Ergebnisse Wertebereich", 
            value = "range_results_panel", 
            reactable::reactableOutput(outputId = "results_range_tbl")
          )
        )
        
      )
      
    )
    
  ), 
  
  ## 2.5 "Metrics" Page ----
  shiny::tabPanel(
    title = "persönliche Leistung", 
    
    shiny::fluidRow(
      
      shiny::column(
        width = 6, 
        
        shiny::h2("Leistung bei binären Fragen"), 
        echarts4r::echarts4rOutput(outputId = "binary_metrics_chart")
      ), 
      
      shiny::column(
        width = 6, 
        
        shiny::h2("Leistung bei Wertebereichen"), 
        echarts4r::echarts4rOutput(outputId = "range_metrics_chart")
      )
    ), 
    
    shiny::hr(), 
    
    shiny::fluidRow(
      shiny::h4(
        class = "text-center", 
        "Das Ziel (für beide Diagramme auf dieser Seite) ist, dass der blaue Balken die grüne Linie trifft."
      )
    )
    
  ), 
  
  ## 2.6 "Help" Page ----
  shiny::tabPanel(
    title = "Hilfe", 
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
        "src='https://www.youtube.com/embed/7P2YI9-smfU'", 
        "title='YouTube video player' frameborder='0' allow='accelerometer;", 
        "autoplay; clipboard-write; encrypted-media; gyroscope;", 
        "picture-in-picture' allowfullscreen></iframe>", 
        .sep = " "
      )
    )
    
  )
  
)



# 3.0 SERVER ----
server <- function(input, output, session) {
  
  ## 3.1 Build Waiting Screen ----
  w <- waiter::Waiter$new(
    id = c("question_ui"), 
    html = shiny::tagList(
      waiter::spin_flower(), 
      "Loading Next Question..."
    ), 
    color = "#153015",
  )
  
  # Setup initial reactiveValues
  rctv <- shiny::reactiveValues(
    
    # Create the reactive data frame holding user's "binary" question responses
    binary_tbl = data.frame(
      Group = as.integer(), 
      Frage = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Antwort = as.character(), 
      Konfidenz = as.character(), 
      Loesung = as.character(), 
      Brier = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    ), 
    
    # Create the reactive data frame holding user's "range" question responses
    range_tbl = data.frame(
      Group = as.integer(), 
      Frage = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Untere90 = as.numeric(), 
      Obere90 = as.numeric(), 
      korr.Antwort = as.numeric(), 
      RelativeError = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    )
  )
  
  ## 3.2 User Info Modal ----
  # On app launch, display pop-up modal for user to enter first & last name
  shiny::modalDialog(
    title = "Benutzerinformation", 
    shiny::tagList(
      shiny::div(
        shiny::textInput(
          inputId = "user_first_name", 
          label = "Vorname", 
          placeholder = "Vorname"
        ), 
        shiny::textInput(
          inputId = "user_last_name", 
          label = "Nachname", 
          placeholder = "Nachname"
        )
      )
    ), 
    easyClose = FALSE, 
    footer = shiny::tagList(
      shiny::div(
        # Button to submit user's information
        shiny::actionButton(
          inputId = "submit_user_info_btn", 
          label = "Absenden", 
          icon = shiny::icon("check")
        )
      )
    )
  ) |> 
    shiny::showModal()
  
  ## 3.3 "Submit User Info" Button ----
  # When the "Submit" button is clicked in the user info pop-up modal...
  shiny::observeEvent(input$submit_user_info_btn, {
    
    # require that the "First Name" and "Last Name" fields have been populated
    shiny::req(
      input$user_first_name, 
      input$user_last_name
    )
    
    # remove the open modal dialogue
    shiny::removeModal()
    
    # list any existing {pins} history for this user
    user_history <- pins::pin_search(
      board = board, 
      search = glue::glue("{input$user_last_name}_{input$user_first_name}")
    )
    
    # if the user has completed a group (that wrote successfully to a pin)...
    if (nrow(user_history) > 0) {
      
      # ... get the highest group number that was completed
      last_group_completed <- user_history %>% 
        split(.$name) %>% 
        purrr::map_dfr(
          function(x) pins::pin_read(board = board, name = x$name) |> dplyr::select(Group), 
          .id = "source"
        ) %>% 
        dplyr::pull(Group) |> 
        max()
      
      # set the current group number to the *next* group
      rctv$current_group_number <- last_group_completed + 1
      
      # retrieve the corresponding current question type
      rctv$current_question_type <- question_index$QuestionType[question_index$Group == rctv$current_group_number][1]
      
      # lookup the first question (index) for the *next* group
      rctv$current_question_number <- question_index$Index[question_index$Group == rctv$current_group_number & question_index$QuestionNumber == 1 & question_index$QuestionType == rctv$current_question_type]
      
      # update the reactive 'binary_tbl' and 'range_tbl' with the user's history
      binary_history <- pins::pin_read(
        board = board, 
        name = glue::glue("binary_{input$user_last_name}_{input$user_first_name}")
      )
      
      range_history <- pins::pin_read(
        board = board, 
        name = glue::glue("range_{input$user_last_name}_{input$user_first_name}")
      )
      
      rctv$binary_tbl <- rctv$binary_tbl |> 
        dplyr::bind_rows(
          binary_history |> dplyr::select(-User)
        )
      
      rctv$range_tbl <- rctv$range_tbl |> 
        dplyr::bind_rows(
          range_history |> dplyr::select(-User)
        )
      
    } else {
      
      # ... otherwise, if no user history (pin) was found, start at the beginning
      rctv$current_group_number = 1
      
      rctv$current_question_number = 1
      
      rctv$current_question_type <- question_index$QuestionType[1]
      
    }
    
  })
  
  ## 3.5 "Next" Button ----
  # When the "Next" button is clicked...
  shiny::observeEvent(input$next_btn, {
    
    # Capture the current response / Lower90
    rctv$current_response_1 <- eval(
      parse(text = glue::glue(
        "input$group_{rctv$current_group_number}_", 
        "{rctv$current_question_type}_answer_", 
        "{question_index$QuestionNumber[rctv$current_question_number]}_ui_A"
      ))
    )
    
    # Capture the current Konfidenz / Upper90 
    rctv$current_response_2 <- eval(
      parse(text = glue::glue(
        "input$group_{rctv$current_group_number}_", 
        "{rctv$current_question_type}_answer_", 
        "{question_index$QuestionNumber[rctv$current_question_number]}_ui_B"
      ))
    )
    
    # For "range" type questions, ensure that the entry for "Lower90" is less 
    # than the value entered for "Upper90"
    if ((rctv$current_question_type == "range") & (rctv$current_response_1 >= rctv$current_response_2)) {
      
      # Build a modal asking the user to fix the issue
      modal <- shiny::modalDialog(
        title = "Bitte den folgenden Fehler beheben", 
        paste0(
          "Der eingegebene Wert für \"Untere Grenze\" muss kleiner sein als der Wert", 
          " für  \"Obere Grenze\"."
        ), 
        footer = shiny::modalButton(
          label = "Zurück", 
          icon = shiny::icon("pen")
        )
      )
      
    } else {
      
      # Create the first modal text segment
      modal_text_1 <- ifelse(
        rctv$current_question_type == "binary", 
        "Ihre Antwort:", 
        "Ihre Atwort (untere 90%):"
      )
      
      # Create the second modal text segment
      modal_text_2 <- ifelse(
        rctv$current_question_type == "binary", 
        "Mit Konfidenz:", 
        "Ihre Antwort (obere 90%):"
      )
      
      # Create the modal text suffix
      modal_text_3 <- ifelse(
        rctv$current_question_type == "binary", 
        "%", 
        ""
      )
      
      # Build a modal asking user to confirm their answer
      modal <- shiny::modalDialog(
        title = "Sind Sie sicher?", 
        glue::glue("{modal_text_1} {rctv$current_response_1}"), 
        shiny::br(), 
        glue::glue("{modal_text_2} {rctv$current_response_2}{modal_text_3}"), 
        easyClose = FALSE, 
        footer = shiny::tagList(
          shiny::div(
            # Button to dismiss the modal
            shiny::modalButton(
              label = "Zurück", 
              icon = shiny::icon("pen")
            ), 
            # Button to move to the next question
            shiny::actionButton(
              inputId = "submit_answer_btn", 
              label = "Absenden", 
              icon = shiny::icon("check")
            )
          )
        )
      )
      
    }
    
    # Launch the modal pop-up
    shiny::showModal(modal)
    
  })
  
  ## 3.6 "Submit Answer" Button ----
  # When the "Submit" button is clicked...
  shiny::observeEvent(input$submit_answer_btn, {
    
    # ... remove the open modal dialogue
    shiny::removeModal()
    
    # Show the waiting screen
    w$show()
    
    # Append a new row to the reactive "binary" or "range" data frame 
    if (rctv$current_question_type == "binary") {
      
      current_question <- question_index %>% 
        dplyr::filter(Index == rctv$current_question_number) %>% 
        dplyr::select(Group, QuestionNumber) %>% 
        dplyr::mutate(Group = paste0("Group_", Group)) %>% 
        dplyr::inner_join(
          questions$binary, 
          by = c("Group", "QuestionNumber")
        )
      
      rctv$binary_tbl <- rctv$binary_tbl |>
        rbind(
          data.frame(
            Group = rctv$current_group_number, 
            Frage = current_question$QuestionNumber, 
            QuestionText = current_question$Question, 
            Index = rctv$current_question_number, 
            Antwort = rctv$current_response_1, 
            Konfidenz = paste0(rctv$current_response_2, "%"), 
            Loesung = current_question$Answer, 
            Brier = brier(
              response = stringr::str_sub(rctv$current_response_1, 1L, 1L), 
              confidence = (rctv$current_response_2 / 100), 
              correct_answer = current_question$Answer
            ), 
            Source = current_question$Source_link, 
            stringsAsFactors = FALSE
          )
        )
      
    } else {
      
      current_question <- question_index %>% 
        dplyr::filter(Index == rctv$current_question_number) %>% 
        dplyr::select(Group, QuestionNumber) %>% 
        dplyr::mutate(Group = paste0("Group_", Group)) %>% 
        dplyr::inner_join(
          questions$range, 
          by = c("Group", "QuestionNumber")
        )
      
      rctv$range_tbl <- rctv$range_tbl |>
        rbind(
          data.frame(
            Group = rctv$current_group_number, 
            Frage = current_question$QuestionNumber, 
            QuestionText = current_question$Question, 
            Index = rctv$current_question_number, 
            Untere90 = rctv$current_response_1, 
            Obere90 = rctv$current_response_2, 
            korr.Antwort = current_question$Answer, 
            RelativeError = relative_error(
              lower_90 = rctv$current_response_1,
              upper_90 = rctv$current_response_2,
              correct_answer = current_question$Answer
            ), 
            Source = current_question$Source_link, 
            stringsAsFactors = FALSE
          )
        )
      
    }
    
    # If the submission was the last question in the *entire* workshop...
    if (rctv$current_question_number == max(question_index$Index)) {
      
      # Write out the current results to the user's pin
      write_to_pin(
        board = board, 
        type = "binary", 
        data = rctv$binary_tbl, 
        user_first = trimws(input$user_first_name), 
        user_last = trimws(input$user_last_name)
      )
      
      write_to_pin(
        board = board, 
        type = "range", 
        data = rctv$range_tbl, 
        user_first = trimws(input$user_first_name), 
        user_last = trimws(input$user_last_name)
      )
      
      # Hide the {waiter} loading screen
      w$hide()
      
      # Remove the "question_ui" output element (to keep it from overlapping 
      # the pop-up modal we create next)
      shiny::removeUI(
        selector = "div:has(> #question_ui)", 
        immediate = TRUE
      )
      
      # Launch a pop-up modal letting the user know they have completed the 
      # workshop
      shiny::modalDialog(
        title = "Fertig!", 
        glue::glue(
          "Sie haben das Kalibrierungstraining erfolgreich abgeschlossen.", 
        ), 
        shiny::br(), 
        "Bitte warten Sie auf die Workshopleitung bevor sie gehen.", 
        size = "l"
      ) |> 
        shiny::showModal()
      
    } else {
      
      # Increase the 'current_question_number' value by 1
      rctv$current_question_number <- rctv$current_question_number + 1
      
      # Get the corresponding group number for the next question
      rctv$current_group_number <- question_index$Group[rctv$current_question_number]
      
      # Get the corresponding question type for the next question
      rctv$current_question_type <- question_index$QuestionType[rctv$current_question_number]
      
      # If the new question switches from "binary" to "range" (or vice versa), 
      # change the "Tables" tab to show the current table
      if (question_index$QuestionType[rctv$current_question_number] != question_index$QuestionType[rctv$current_question_number - 1]) {
        
        shiny::updateTabsetPanel(
          session = session, 
          inputId = "results_tabset", 
          selected = paste0(rctv$current_question_type, "_results_panel")
        )
        
      }
      
      # If the new question begins a new group, write the most current results to
      # {pins} database and show a pop-up
      if (question_index$Group[rctv$current_question_number] != question_index$Group[rctv$current_question_number - 1]) {
        
        
        rctv$binary_tbl_backend <- rctv$binary_tbl %>% 
            rename_at( 2, ~"Question") %>%
            rename_at( 5, ~"Response") %>%
            rename_at( 6, ~"Confidence") %>%
            rename_at( 7, ~"Truth")
          
        
        write_to_pin(
          board = board, 
          type = "binary", 
          data = rctv$binary_tbl_backend, 
          user_first = trimws(input$user_first_name), 
          user_last = trimws(input$user_last_name)
        )
        
        rctv$range_tbl_backend <- rctv$range_tbl %>% 
          rename_at( 2, ~"Question") %>%
          rename_at( 5, ~"Lower90") %>%
          rename_at( 6, ~"Upper90") %>%
          rename_at( 7, ~"Truth")
        
        write_to_pin(
          board = board, 
          type = "range", 
          data = rctv$range_tbl_backend, 
          user_first = trimws(input$user_first_name), 
          user_last = trimws(input$user_last_name)
        )
        
        # Show a "Group Complete" pop-up modal
        shiny::modalDialog(
          title = "Fragenset fertig!", 
          glue::glue(
            "Sie haben Fragenset {rctv$current_group_number - 1} erfolgreicht abgeschlossen.", 
          ), 
          footer = modalButton("Fortfahren"),
          shiny::br(), 
          "Bitte warten Sie auf die Anweisungen der Workshopleitung bevor sie fortfahren.", 
          size = "l"
        )|> 
          shiny::showModal()
        
      }
      
    }
    
  })
  
  
  ## 3.7 Render Question & Response UI  ----
  output$question_ui <- shiny::renderUI({
    
    # Require the current question type, group number, and question number
    shiny::req(
      rctv$current_question_type, 
      rctv$current_group_number, 
      rctv$current_question_number
    )
    
    
    # Display the appropriate UI response elements based on the current question
    # type
    if (rctv$current_question_type == "binary") {
      
      binary_ui %>% 
        purrr::pluck(
          glue::glue("Group_{rctv$current_group_number}"), 
          glue::glue("question_{question_index$QuestionNumber[rctv$current_question_number]}")
        )
      
    } else {
      
      range_ui %>% 
        purrr::pluck(
          glue::glue("Group_{rctv$current_group_number}"), 
          glue::glue("question_{question_index$QuestionNumber[rctv$current_question_number]}")
        )
      
    }
    
  })
  
  ## 3.8 Binary Results Table ----
  # Create the table to hold the "Binary" results & scores
  output$results_binary_tbl <- reactable::renderReactable({
    
    # Require the "binary" response table
    shiny::req(rctv$binary_tbl)
    
    data <- rctv$binary_tbl %>% 
      dplyr::filter(Group == rctv$current_group_number)
    
    # Populate the interactive table with the "binary" data from the current 
    # question group
    reactable::reactable(
      data, 
      columns = list(
        Frage = reactable::colDef(cell = function(value, index) {
          hover <- data[index, "QuestionText"]
          # Render as text that can be hovered over to show full question
          htmltools::tags$span(
            title = hover,
            value
          )
        }),
        Group = reactable::colDef(show = FALSE), 
        Index = reactable::colDef(show = FALSE), 
        Brier = reactable::colDef(
          format = reactable::colFormat(digits = 2)
        ), 
        Loesung = reactable::colDef(cell = function(value, index) {
          text <- if (value == "T") "Richtig" else "Falsch"
          url <- data[index, "Source"]
          # Render as a link
          htmltools::tags$a(
            href = url, 
            target = "_blank", 
            text
          )
        }), 
        QuestionText = reactable::colDef(show = FALSE), 
        Source = reactable::colDef(show = FALSE)
      ), 
      theme = reactable::reactableTheme(
        backgroundColor = "#153015"
      )
    )
    
  })
  
  ## 3.9 Range Results Table ----
  # Create the table to hold the "Range" results & scores
  output$results_range_tbl <- reactable::renderReactable({
    
    # Require the "range" response table
    shiny::req(rctv$range_tbl)
    
    data <- rctv$range_tbl %>% 
      dplyr::filter(Group == rctv$current_group_number)
    
    # Populate the interactive table with the "range" data from the current 
    # question group
    reactable::reactable(
      data, 
      columns = list(
        Frage = reactable::colDef(cell = function(value, index) {
          hover <- data[index, "QuestionText"]
          # Render as text that can be hovered over to show full question
          htmltools::tags$span(
            title = hover,
            value
          )
        }),
        Group = reactable::colDef(show = FALSE), 
        Index = reactable::colDef(show = FALSE), 
        Untere90 = reactable::colDef(name = "Untere Grenze"),
        Obere90 = reactable::colDef(name = "Obere Grenze"), 
        RelativeError = reactable::colDef(
          name = "Relativer Fehler", 
          format = reactable::colFormat(digits = 2)
        ), 
        korr.Antwort = reactable::colDef(cell = function(value, index) {
          url <- data[index, "Source"]
          # hover <- data[index, "Comments"]
          # Render as a link
          htmltools::tags$a(
            # title = hover, 
            href = url, 
            target = "_blank", 
            value
          )
        }), 
        QuestionText = reactable::colDef(show = FALSE), 
        Source = reactable::colDef(show = FALSE)
      ), 
      columnGroups = list(
        reactable::colGroup(
          name = "90% Konfidenzintervall", 
          columns = c("Untere90", "Obere90")
        )
      ), 
      theme = reactable::reactableTheme(
        backgroundColor = "#153015"
      )
    )
    
  })
  
  ## 3.10 Binary Metrics Chart ----
  # Create the chart to hold the binary metrics
  output$binary_metrics_chart <- echarts4r::renderEcharts4r({
    
    # Require that there is data in the "binary" response table
    shiny::req(nrow(rctv$binary_tbl) > 0)
    
    # Create the "binary" metrics interactive chart
    generate_binary_metrics_chart(
      data = rctv$binary_tbl
    )
    
  })
  
  ## 3.11 Range Metrics Chart ----
  # Create the chart to hold the range metrics
  output$range_metrics_chart <- echarts4r::renderEcharts4r({
    
    # Require that there is data in the "range" response table
    shiny::req(nrow(rctv$range_tbl) > 0)
    
    # Create the "range" metrics interactive chart
    generate_range_metrics_chart(
      data = rctv$range_tbl
    )
    
  })
  
}

shinyApp(ui, server)
