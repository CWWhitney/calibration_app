##******************************************************************************
##*
##* This is the questions_page shiny module which description
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
#' @seealso [mod_questions_page_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'
#'    shiny::shinyApp(
#'      ui = fluidPage(
#'        mod_questions_page_ui(
#'          id = "mod_questions_page"
#'        )
#'      ),
#'      server = function(input, output, session) {
#'        mod_questions_page_server(
#'          id ="mod_questions_page"
#'        )
#'      }
#'    )
#'  
#'  }
mod_questions_page_ui <- function(id, tab_title, next_btn_label, binary_results_panel_title, range_results_panel_title) {
  ns <- NS(id)
  shiny::tabPanel(
    title = tab_title, 
    
    shiny::fluidRow(
      ### 2.3 Questions UI Elements ----
      shiny::column(
        width = 6, 
        shiny::wellPanel(
          style = "background: #153015;", 
          
          #### 2.3.1 Question & Response UI ----
          shiny::uiOutput(outputId = ns("question_ui")), 
          
          shiny::hr(), 
          
          #### 2.3.4 Previous / Next Buttons ----
          # Create a button to go back to the prior question
          shiny::div(
            style = "float: right;",
            shiny::actionButton(
              class = "btn btn-lg", 
              inputId = ns("next_btn"), 
              label = next_btn_label, 
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
            title = binary_results_panel_title, 
            value = "binary_results_panel", 
            reactable::reactableOutput(outputId = ns("results_binary_tbl"))
          ), 
          
          #### 2.4.1 "Range" Results Table ----
          shiny::tabPanel(
            title = range_results_panel_title, 
            value = "range_results_panel", 
            reactable::reactableOutput(outputId = ns("results_range_tbl"))
          )
        )
        
      )
    )
  )
}



#' @param id The namespace id of the module.
#'
#' @return `list` of `shiny::reactive({})`s
#'
#' @seealso [questions_page_ui()]
#'
#' @export
#'
#' @inherit mod_questions_page_ui title description details examples
mod_questions_page_server <- function(
    id,
    current_question_type,
    current_group_number,
    current_question_number,
    question_index
    ) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$question_ui <- shiny::renderUI({
        
        # Require the current question type, group number, and question number
        shiny::req(
          current_question_type(), 
          current_group_number(), 
          current_question_number()
        )
        
        # Display the appropriate UI response elements based on the current question
        # type
        if (current_question_type() == "binary") {
          
          binary_ui %>% 
            purrr::pluck(
              glue::glue("Group_{current_group_number()}"), 
              glue::glue("question_{question_index$QuestionNumber[current_question_number()]}")
            )
          
        } else {
          
          range_ui %>% 
            purrr::pluck(
              glue::glue("Group_{current_group_number()}"), 
              glue::glue("question_{question_index$QuestionNumber[current_question_number()]}")
            )
          
        }
        
      })
      
      ## 3.5 "Next" Button ----
      # When the "Next" button is clicked...
      shiny::observeEvent(input$next_btn, {
        #shinyjs::disable("next_btn")
        
        
        
        # # Capture the current response / Lower90
        # rctv$current_response_1 <- eval(
        #   parse(text = glue::glue(
        #     "input$group_{rctv$current_group_number}_", 
        #     "{rctv$current_question_type}_answer_", 
        #     "{question_index$QuestionNumber[rctv$current_question_number]}_ui_A"
        #   ))
        # )
        # 
        # # Capture the current Confidence / Upper90 
        # rctv$current_response_2 <- eval(
        #   parse(text = glue::glue(
        #     "input$group_{rctv$current_group_number}_", 
        #     "{rctv$current_question_type}_answer_", 
        #     "{question_index$QuestionNumber[rctv$current_question_number]}_ui_B"
        #   ))
        # )
        
        # Capture the current response / Lower90
        current_response_1 <- eval(
          parse(text = glue::glue(
            "input$group_{current_group_number()}_", 
            "{current_question_type()}_answer_", 
            "{question_index$QuestionNumber[current_question_number()]}_ui_A"
          ))
        )
        
        # Capture the current Confidence / Upper90 
        current_response_2 <- eval(
          parse(text = glue::glue(
            "input$group_{current_group_number()}_", 
            "{current_question_type()}_answer_", 
            "{question_index$QuestionNumber[current_question_number()]}_ui_B"
          ))
        )
        
        # For "range" type questions, ensure that the entry for "Lower90" is less 
        # than the value entered for "Upper90"
        # if ((rctv$current_question_type == "range") & (rctv$current_response_1 >= rctv$current_response_2)) {
        #   
        #   # Build a modal asking the user to fix the issue
        #   modal <- shiny::modalDialog(
        #     title = selected_language[15], 
        #     paste0(
        #       selected_language[16], selected_language[17]
        #     ), 
        #     footer = shiny::modalButton(
        #       label = selected_language[18], 
        #       icon = shiny::icon("pen")
        #     )
        #   )
        #   
        # } else {
        
          # Create the first modal text segment
          modal_text_1 <- ifelse(
            current_question_type() == "binary", 
            selected_language[19], 
            selected_language[20]
          )
          
          # Create the second modal text segment
          modal_text_2 <- ifelse(
            current_question_type() == "binary", 
            selected_language[21], 
            selected_language[22]
          )
          
          # Create the modal text suffix
          modal_text_3 <- ifelse(
            current_question_type() == "binary", 
            "%", 
            ""
          )
          
          # Build a modal asking user to confirm their answer
          modal <- shiny::modalDialog(
            title = selected_language[23], 
            glue::glue("{modal_text_1} {current_response_1}"), 
            shiny::br(), 
            glue::glue("{modal_text_2} {current_response_2}{modal_text_3}"), 
            easyClose = FALSE, 
            footer = shiny::tagList(
              shiny::div(
                # Button to dismiss the modal
                shiny::modalButton(
                  label = selected_language[18], 
                  icon = shiny::icon("pen")
                ), 
                # Button to move to the next question
                shiny::actionButton(
                  inputId = ns("submit_answer_btn"), 
                  label = selected_language[14], 
                  icon = shiny::icon("check")
                )
              )
            )
          )
          
        # }
        
        # Launch the modal pop-up
        shiny::showModal(modal)
        #shinyjs::enable("next_btn")
      })
      
      
      ## 3.6 "Submit Answer" Button ----
      # When the "Submit" button is clicked...
      shiny::observeEvent(input$submit_answer_btn, {
        
        # ... remove the open modal dialogue
        shiny::removeModal()
        browser()
        # Show the waiting screen
        w$show()
        
        # Append a new row to the reactive "binary" or "range" data frame 
        if (current_question_type() == "binary") {
          
          current_question <- question_index %>% 
            dplyr::filter(Index == current_question_number()) %>% 
            dplyr::select(Group, QuestionNumber) %>% 
            dplyr::mutate(Group = paste0("Group_", Group)) %>% 
            dplyr::inner_join(
              questions$binary, 
              by = c("Group", "QuestionNumber")
            )
          
          
          rctv$binary_tbl <- rctv$binary_tbl |>
            rbind(
              data.frame(
                Group = current_group_number(), 
                Question = current_question$QuestionNumber, 
                QuestionText = current_question$Question, 
                Index = current_question_number(), 
                Response = current_response_1, 
                Confidence = paste0(current_response_2, "%"), 
                Truth = current_question$Answer, 
                Brier = brier(
                  #response = stringr::str_sub(current_response_1, 1L, 1L),
                  response = ifelse(current_response_1 == selected_language[31], "T", "F"),
                  confidence = (current_response_2 / 100), 
                  correct_answer = current_question$Answer
                ), 
                Source = current_question$Source_link, 
                stringsAsFactors = FALSE
              )
            )
          
          
          
        } else {
          
          current_question <- question_index %>% 
            dplyr::filter(Index == current_question_number()) %>% 
            dplyr::select(Group, QuestionNumber) %>% 
            dplyr::mutate(Group = paste0("Group_", Group)) %>% 
            dplyr::inner_join(
              questions$range, 
              by = c("Group", "QuestionNumber")
            )
          
          rctv$range_tbl <- rctv$range_tbl |>
            rbind(
              data.frame(
                Group = current_group_number(), 
                Question = current_question$QuestionNumber, 
                QuestionText = current_question$Question, 
                Index = current_question_number(), 
                Lower90 = current_response_1, 
                Upper90 = current_response_2, 
                Truth = current_question$Answer, 
                RelativeError = relative_error(
                  lower_90 = current_response_1,
                  upper_90 = current_response_2,
                  correct_answer = current_question$Answer
                ), 
                Source = current_question$Source_link, 
                stringsAsFactors = FALSE
              )
            )
          
        }
        
        
        
        browser("here")
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
            title = selected_language[24], 
            glue::glue(
              selected_language[25], 
            ), 
            shiny::br(), 
            selected_language[26], 
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
              title = selected_language[27], 
              glue::glue(
                selected_language[28], 
              ), 
              footer = modalButton(selected_language[29]),
              shiny::br(), 
              selected_language[30], 
              size = "l"
            )|> 
              shiny::showModal()
            
          }
          
        }
        
      })
      
    }
  )
}