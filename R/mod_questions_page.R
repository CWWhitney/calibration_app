##******************************************************************************
##* 
##* This is the questions_page shiny module which provides the UI for displaying
##* questions and capturing user responses.
##* 
##******************************************************************************

#' Questions Page UI Module
#'
#' This module creates the UI for the questions page, including the question 
#' display, navigation buttons, and results panels.
#'
#' @param id The namespace id of the module.
#' @param tab_title The title of the tab panel.
#' @param binary_results_panel_title The title for the binary results panel.
#' @param range_results_panel_title The title for the range results panel.
#'
#' @return An HTML element for use in a UI.
#'
#' @seealso [mod_questions_page_server()]
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       mod_questions_page_ui(
#'         id = "mod_questions_page",
#'         tab_title = "Questions",
#'         binary_results_panel_title = "Binary Results",
#'         range_results_panel_title = "Range Results"
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       mod_questions_page_server(
#'         id = "mod_questions_page",
#'         current_question_type = reactive("binary"),
#'         current_group_number = reactive(1),
#'         current_question_number = reactive(1),
#'         question_index = data.frame(Group = 1, QuestionNumber = 1, QuestionType = "binary", Index = 1),
#'         rctv = reactiveValues(),
#'         issue_dialog_title = "Please Fix the Following Error",
#'         issue_dialog_text = "The lower bound must be less than the upper bound. Please correct your input.",
#'         issue_dialog_button = "Edit",
#'         confirm_dialog_title = "Confirm Your Answer",
#'         confirm_dialog_button = "Submit",
#'         word_for_question = "Question",
#'         word_for_answer = "Answer",
#'         word_for_correct = "Correct",
#'         word_for_incorrect = "Incorrect",
#'         word_for_confidence = "Confidence",
#'         word_for_confidence_interval = "Confidence Interval",
#'         word_for_lower_bound = "Lower Bound",
#'         word_for_upper_bound = "Upper Bound",
#'         next_btn_label = "Next"
#'       )
#'     }
#'   )
#' }
mod_questions_page_ui <- function(id, tab_title, binary_results_panel_title, range_results_panel_title) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = tab_title, 
    bslib::layout_column_wrap(
      width = 1/2,
      shiny::uiOutput(outputId = ns("question_ui")),
      bslib::navset_card_tab(
        id = ns("results_tabset"),
        #### Binary Results Table --------------------------------------------
        bslib::nav_panel(
          title = binary_results_panel_title, 
          value = "binary_results_panel", 
          reactable::reactableOutput(outputId = ns("results_binary_tbl"))
        ), 
        
        #### Range Results Table ---------------------------------------------
        bslib::nav_panel(
          title = range_results_panel_title, 
          value = "range_results_panel", 
          reactable::reactableOutput(outputId = ns("results_range_tbl"))
        )
      )
    )
  )
}




#' @param id The namespace id of the module.
#' @param current_question_type A reactive expression for the current question type.
#' @param current_group_number A reactive expression for the current group number.
#' @param current_question_number A reactive expression for the current question number.
#' @param question_index A data frame containing the question index.
#' @param rctv A reactive values object for storing responses.
#' @param issue_dialog_title The title for the issue dialog.
#' @param issue_dialog_text The text for the issue dialog.
#' @param issue_dialog_button The label for the issue dialog button.
#' @param confirm_dialog_title The title for the confirmation dialog.
#' @param confirm_dialog_button The label for the confirmation dialog button.
#' @param word_for_question The word for "question".
#' @param word_for_answer The word for "answer".
#' @param word_for_correct The word for "correct".
#' @param word_for_incorrect The word for "incorrect".
#' @param word_for_confidence The word for "confidence".
#' @param word_for_confidence_interval The word for "confidence interval".
#' @param word_for_lower_bound The word for "lower bound".
#' @param word_for_upper_bound The word for "upper bound".
#' @param next_btn_label The label for the next button.
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
    user_first_name,
    user_last_name,
    user_session,
    workshop_selection,
    # current_question_type,
    current_group_number,
    current_question_number,
    question_index,
    questions,
    rctv,
    selected_language_rctv,
    issue_dialog_title,
    issue_dialog_text_NA,
    issue_dialog_text_small_large,
    issue_dialog_button,
    round_finished_dialog_text,
    confirm_dialog_title,
    confirm_dialog_button,
    word_for_question,
    word_for_answer,
    word_for_correct,
    word_for_incorrect,
    word_for_confidence,
    word_for_confidence_interval,
    word_for_lower_bound,
    word_for_upper_bound,
    next_btn_label,
    modal_text_1_binary,
    modal_text_1_range,
    modal_text_2_binary,
    modal_text_2_range,
    completion_dialog_title,
    completion_dialog_text_1,
    completion_dialog_text_2,
    first_group_dialog_title,
    first_group_dialog_text_videos,
    first_group_dialog_text_no_videos,
    group_complete_dialog_title,
    group_complete_dialog_text_1,
    group_complete_dialog_button,
    group_complete_dialog_text_2_videos,
    group_complete_dialog_text_2_no_videos
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Build Waiting Screen ---------------------------------------------------
      w <- waiter::Waiter$new(
        id = c("question_ui"), 
        html = shiny::tagList(
          waiter::spin_flower(), 
          "Loading Next Question..."
        ), 
        color = "#153015"
      )
      
      # Question ---------------------------------------------------------------
      current_question_reactive <- reactive({
        # Require the current question type, group number, and question number
        shiny::req(
          question_index(),
          current_group_number(), 
          current_question_number(),
          selected_language_rctv()
        )
        
        current_question_index <- question_index() |> 
          dplyr::filter(Index == current_question_number()) |> 
          dplyr::select(Group, QuestionNumber, QuestionType) |> 
          dplyr::mutate(Group = paste0("Group_", Group))
        
        current_question <- current_question_index |> 
          dplyr::inner_join(
            questions() |> purrr::pluck(current_question_index$QuestionType), 
            by = c("Group", "QuestionNumber")
          )
        
        current_question
        
      })
      
      current_question_type <- 
        reactive({
          current_question_reactive() |> 
            pull("QuestionType")
        })
      
      start_of_round <- reactiveVal(TRUE)
      end_of_workshop <- reactiveVal(FALSE)
      
      # Render Question UI -----------------------------------------------------
      output$question_ui <- shiny::renderUI({
        
        if (isTRUE(end_of_workshop())) {
          
          return(
            bslib::card(
              bslib::card_header(h3(interface_translator$t(completion_dialog_title)), class = "bg-dark"),
              glue::glue(
                interface_translator$t(completion_dialog_text_1)
              ),
              shiny::br(),
              interface_translator$t(completion_dialog_text_2)
            )
          )
        }
        
        question <- current_question_reactive() |> 
          dplyr::pull(
            selected_language_rctv()
          )
        
        if (current_question_type() == "binary") {
          question <- binary_translator$t(question)
        }
        
        if(current_question_type() == "range") {
          question <- range_translator$t(question)
        }
        
        
        if (isTRUE(start_of_round())) { 
          
          ## Show a "Group Complete" pop-up modal
          help_videos_active <- load_question_sets() |> 
            dplyr::filter(question_set_name == workshop_selection()) |>
            dplyr::pull("help_videos_active") |>
            as.logical()
          
          if((rctv$current_group_number - 1) == 0) {
            
            return(
              bslib::card(
                bslib::card_header(h3(interface_translator$t(first_group_dialog_title)), class = "bg-dark"),
                
                if(help_videos_active) {
                  shiny::tagList(
                    interface_translator$t(first_group_dialog_text_videos),
                    help_video_function(rctv$current_group_number)
                  )
                } else {
                  interface_translator$t(first_group_dialog_text_no_videos)
                },
                bslib::card_footer(
                  class = "bg-dark d-flex justify-content-end",
                  shiny::actionButton(
                    class = "btn btn-lg", 
                    inputId = ns("next_round"), 
                    label = interface_translator$t(group_complete_dialog_button), 
                    icon = shiny::icon(name = "arrow-right")
                  )
                )
              )
            )
          } else {

            return(
              bslib::card(
                bslib::card_header(h3(interface_translator$t(group_complete_dialog_title)), class = "bg-dark"),
                glue::glue(
                  interface_translator$t(group_complete_dialog_text_1)
                ),
                if(help_videos_active) {
                  shiny::tagList(
                    help_video_function(rctv$current_group_number),
                    interface_translator$t(group_complete_dialog_text_2_videos)
                  )
                } else {
                  interface_translator$t(group_complete_dialog_text_2_no_videos)
                },
                bslib::card_footer(
                  class = "bg-dark d-flex justify-content-end",
                  shiny::actionButton(
                    class = "btn btn-lg", 
                    inputId = ns("next_round"), 
                    label = interface_translator$t(group_complete_dialog_button), 
                    icon = shiny::icon(name = "arrow-right")
                  )
                )
              )
            )
          } 
        }
        
        
        # Display the appropriate UI response elements based on the current question type
        return(
          mod_question_ui(
            ns("question"),
            question = question,
            question_row_number = current_question_number(),
            type = current_question_type(),
            word_for_question = interface_translator$t(word_for_question),
            word_for_answer = interface_translator$t(word_for_answer),
            word_for_correct = interface_translator$t(word_for_correct),
            word_for_incorrect = interface_translator$t(word_for_incorrect),
            word_for_confidence = interface_translator$t(word_for_confidence),
            word_for_confidence_interval = interface_translator$t(word_for_confidence_interval),
            word_for_lower_bound = interface_translator$t(word_for_lower_bound),
            word_for_upper_bound = interface_translator$t(word_for_upper_bound),
            next_btn_label = interface_translator$t(next_btn_label)
          )
        )
      })
      
      observeEvent(input$next_round, {
        
        proceed_round <- load_question_sets() |> 
          dplyr::filter(question_set_name == workshop_selection()) |>
          dplyr::pull(stringr::str_c("round_", rctv$current_group_number)) |>
          as.logical()
        
        if (isFALSE(proceed_round)) {
          ## Build a modal to not proceed the round if the instructor has not enabled it.
          modal <- shiny::modalDialog(
            title = interface_translator$t(issue_dialog_title),
            interface_translator$t(round_finished_dialog_text),
            easyClose = FALSE,
            footer = shiny::tagList(shiny::div(
              ## Button to dismiss the modal
              shiny::modalButton(
                label = interface_translator$t(issue_dialog_button),
                icon = shiny::icon("pen")
              )
            ))
          )
          
          ## Launch the modal pop-up
          shiny::showModal(modal)
          
        } else {
          start_of_round(FALSE)
          shiny::updateTabsetPanel(
            session = session, 
            inputId = "results_tabset", 
            selected = paste0(current_question_type(), "_results_panel")
          )
        }
      })
      
      # Question Responses -----------------------------------------------------
      question_responses <- reactiveValues(
        A = NULL, 
        B = NULL
      )
      
      ## Start Question Module Server ----------------------------------------
      question_reactives <- 
        mod_question_server(
          "question",
          question_type = current_question_type,
          required_text_label = selected_language[61],
          number_text_label = selected_language[62],
          left_lower_label = selected_language[63]
        )
      
      # Next Button ------------------------------------------------------------
      ## When the "Next" button is clicked...
      shiny::observeEvent(question_reactives$next_btn(), {
        #shinyjs::disable("next_btn")
        
        question_reactives$iv$enable()
        ## Don't proceed if any input is invalid
        req(question_reactives$iv$is_valid())
        
        question_responses$A <- question_reactives$A()
        question_responses$B <- question_reactives$B()
        
        ## Capture the current response / Lower90
        current_response_1 <- question_responses$A 
        
        ## Capture the current Confidence / Upper90 
        current_response_2 <-  question_responses$B
        
        # Create Modal Dialog --------------------------------------------------
        
        if(current_question_type() == "binary") {
          modal_text_1 <- interface_translator$t(modal_text_1_binary)
          modal_text_2 <- interface_translator$t(modal_text_2_binary)
          modal_text_3 <- "%"
          
          if (current_response_1 == "TRUE") {
            current_response_1 <- interface_translator$t(selected_language[31]) 
          } else {
            current_response_1 <- interface_translator$t(selected_language[32])
          }
          
        } else {
          modal_text_1 <- interface_translator$t(modal_text_1_range)
          modal_text_2 <- interface_translator$t(modal_text_2_range)
          modal_text_3 <- ""
        }
        
        ## Build a modal asking user to confirm their answer
        modal <- shiny::modalDialog(
          title = interface_translator$t(confirm_dialog_title), 
          glue::glue("{modal_text_1} {current_response_1}"), 
          shiny::br(), 
          glue::glue("{modal_text_2} {current_response_2}{modal_text_3}"), 
          easyClose = FALSE, 
          footer = shiny::tagList(
            shiny::div(
              ## Button to dismiss the modal
              shiny::modalButton(
                label = interface_translator$t(issue_dialog_button), 
                icon = shiny::icon("pen")
              ), 
              ## Button to move to the next question
              shiny::actionButton(
                inputId = ns("submit_answer_btn"), 
                label = interface_translator$t(confirm_dialog_button), 
                icon = shiny::icon("check")
              )
            )
          )
        )
        
        ## Launch the modal pop-up
        shiny::showModal(modal)
        #shinyjs::enable("next_btn")
      })
      
      
      # Submit Answer Button ---------------------------------------------------
      ## When the "Submit" button is clicked...
      shiny::observeEvent(input$submit_answer_btn, {
        
        question_reactives$iv$disable()
        ## Show the waiting screen
        w$show()
        
        ## ... remove the open modal dialogue
        shiny::removeModal()
        
        ## Capture the current response / Lower90
        current_response_1 <- question_responses$A 
        
        ## Capture the current Confidence / Upper90 
        current_response_2 <-  question_responses$B
        
        ## Append a new row to the reactive "binary" or "range" data frame 
        if (current_question_type() == "binary") {
          
          
          current_binary_tbl <- 
            data.frame(
              Group = current_group_number(), 
              Question = current_question_reactive()$QuestionNumber, 
              QuestionText = current_question_reactive()[["English"]], 
              Index = current_question_number(), 
              Response = current_response_1, 
              Confidence = paste0(current_response_2, "%"), 
              Truth = current_question_reactive()$Answer, 
              Brier = brier(
                response = ifelse(current_response_1 == word_for_correct, "T", "F"),
                confidence = (current_response_2 / 100), 
                correct_answer = current_question_reactive()$Answer
              ), 
              Source = current_question_reactive()$Source_link, 
              stringsAsFactors = FALSE
            )
          
          insert_binary_response(
            pool = pool,
            user_first_name = user_first_name(),
            user_last_name = user_last_name(),
            user_session = user_session(),
            workshop_set = workshop_selection(),
            round_number = current_group_number(),
            question_number = current_question_reactive()$QuestionNumber,
            question_text = current_question_reactive()[["English"]],
            index_in_set = current_question_number(),
            response = current_response_1,
            confidence = paste0(current_response_2, "%"),
            truth = current_question_reactive()$Answer,
            brier_score = brier(
              response = ifelse(current_response_1 == word_for_correct, "T", "F"),
              confidence = (current_response_2 / 100),
              correct_answer = current_question_reactive()$Answer
            )
          )
          
          rctv$binary_tbl <- rctv$binary_tbl |>
            rbind(current_binary_tbl)
          
        } else {
          
          current_range_tbl <- data.frame(
            Group = current_group_number(), 
            Question = current_question_reactive()$QuestionNumber, 
            QuestionText = current_question_reactive()[[selected_language_rctv()]], 
            Index = current_question_number(), 
            Lower90 = current_response_1, 
            Upper90 = current_response_2, 
            Truth = current_question_reactive()$Answer, 
            RelativeError = relative_error(
              lower_90 = current_response_1,
              upper_90 = current_response_2,
              correct_answer = as.numeric(current_question_reactive()$Answer)
            ), 
            Source = current_question_reactive()$Source_link, 
            stringsAsFactors = FALSE
          )
          
          
          insert_range_response(
            pool = pool,
            user_first_name = user_first_name(),
            user_last_name = user_last_name(),
            user_session = user_session(),
            workshop_set = workshop_selection(),
            round_number = current_group_number(),
            question_number = current_question_reactive()$QuestionNumber,
            question_text = current_question_reactive()[["English"]],
            index_in_set = current_question_number(),
            lower_90 = current_response_1,
            upper_90 = current_response_2,
            truth = current_question_reactive()$Answer,
            relative_error = relative_error(
              lower = current_response_1,
              upper = current_response_2,
              correct_answer = as.numeric(current_question_reactive()$Answer)
            )
          )
          
          
          
          rctv$range_tbl <- rctv$range_tbl |>
            rbind(current_range_tbl)
        }
        
        ## If the question is the last of the workshop
        if (current_question_number() == max(question_index()$Index)) {
          ## If the submission was the last question in the *entire* workshop.
          print("here")
          end_of_workshop(TRUE)
        }
        
        if (current_question_number() <= max(question_index()$Index)) {
          ## Increase the 'current_question_number' value by 1
          rctv$current_question_number <- current_question_number() + 1
          ## Get the corresponding group number for the next question
          rctv$current_group_number <- question_index()$Group[rctv$current_question_number]
          ## Get the corresponding question type for the next question
          # rctv$current_question_type <- question_index()$QuestionType[rctv$current_question_number]
        } 
        
        upsert_user_info(
          pool = pool,
          user_first_name = user_first_name(),
          user_last_name = user_last_name(),
          user_session = user_session(),
          workshop_set = workshop_selection(),
          round_number = rctv$current_group_number,
          question_number = rctv$current_question_number,
          # question_type = rctv$current_question_type
          question_type = current_question_type()
        )
        
        
        ## If the new question switches from "binary" to "range" (or vice versa), 
        ## change the "Tables" tab to show the current table
        if (question_index()$QuestionType[rctv$current_question_number] != question_index()$QuestionType[rctv$current_question_number - 1]) {
          shiny::updateTabsetPanel(
            session = session, 
            inputId = "results_tabset", 
            selected = paste0(current_question_type(), "_results_panel")
          )
        }
        
        ## If the new question begins a new group, write the most current results to
        ## {pins} database and show a pop-up
        if (question_index()$Group[rctv$current_question_number] != question_index()$Group[rctv$current_question_number - 1]) {
          shiny::updateTabsetPanel(
            session = session, 
            inputId = "results_tabset", 
            selected = "binary"
          )
          ## If the submission was the last question in a group.
          start_of_round(TRUE)
        }
        
      })
      
      
      
      # Binary Results Table --------------------------------------------------
      # Create the table to hold the "Binary" results & scores
      output$results_binary_tbl <- reactable::renderReactable({
        selected_language_rctv()
        
        # Require the "binary" response table
        shiny::req(rctv$binary_tbl)
        
        data <- rctv$binary_tbl |>
          dplyr::filter(Group == current_group_number() - isTRUE(start_of_round()))
        
        #colnames(rctv$binary_tbl)[colnames(rctv$binary_tbl) == "Question"] = "interface_translator$t(selected_language[42])"
        
        # Populate the interactive table with the "binary" data from the current
        # question group
        reactable::reactable(
          data,
          pagination = FALSE,
          columns = list(
            Question = reactable::colDef(cell = function(value, index) {
              hover <- data[index, "QuestionText"]
              # Render as text that can be hovered over to show full question
              htmltools::tags$span(
                title = hover,
                value
              )
            }, name = interface_translator$t(selected_language[42])),
            Group = reactable::colDef(show = FALSE),
            Index = reactable::colDef(show = FALSE),
            Response = reactable::colDef(
              cell = function(value, index) {
                text <- if (value == "TRUE") interface_translator$t(selected_language[31]) else interface_translator$t(selected_language[32])
                
                text
              },
              name = interface_translator$t(selected_language[41])
            ),
            Confidence = reactable::colDef(show = TRUE, name = interface_translator$t(selected_language[40])),
            Brier = reactable::colDef(
              format = reactable::colFormat(digits = 2)
            ),
            Truth = reactable::colDef(
              cell = function(value, index) {
                text <- if (value == "T") interface_translator$t(selected_language[31]) else interface_translator$t(selected_language[32])
                url <- data[index, "Source"]
                # Render as a link
                htmltools::tags$a(
                  href = url,
                  target = "_blank",
                  text
                )
              },
              name = interface_translator$t(selected_language[43])
            ),
            QuestionText = reactable::colDef(show = FALSE),
            Source = reactable::colDef(show = FALSE)
          ),
          theme = reactable::reactableTheme(
            backgroundColor = "#153015"
          )
        )
      })
      
      
      # Results Table ---------------------------------------------------------
      # Create the table to hold the "Range" results & scores
      output$results_range_tbl <- reactable::renderReactable({
        selected_language_rctv()
        
        # Require the "range" response table
        shiny::req(rctv$range_tbl)
        
        data <- rctv$range_tbl |>
          dplyr::filter(Group == current_group_number() - isTRUE(start_of_round()))
        
        # Populate the interactive table with the "range" data from the current
        # question group
        reactable::reactable(
          data,
          pagination = FALSE,
          columns = list(
            Question = reactable::colDef(cell = function(value, index) {
              hover <- data[index, "QuestionText"]
              # Render as text that can be hovered over to show full question
              htmltools::tags$span(
                title = hover,
                value
              )
            }, name = interface_translator$t(selected_language[42])),
            Group = reactable::colDef(show = FALSE),
            Index = reactable::colDef(show = FALSE),
            Lower90 = reactable::colDef(name = interface_translator$t(selected_language[33])),
            Upper90 = reactable::colDef(name = interface_translator$t(selected_language[34])),
            RelativeError = reactable::colDef(
              name = interface_translator$t(selected_language[47]),
              format = reactable::colFormat(digits = 2)
            ),
            Truth = reactable::colDef(
              cell = function(value, index) {
                url <- data[index, "Source"]
                
                if(is.na(url)){
                  htmltools::p(value)
                } else {
                  # hover <- data[index, "Comments"]
                  # Render as a link
                  htmltools::tags$a(
                    # title = hover,
                    href = url,
                    target = "_blank",
                    value
                  )
                }
              }, name = interface_translator$t(selected_language[48])),
            QuestionText = reactable::colDef(show = FALSE),
            Source = reactable::colDef(show = FALSE)
          ),
          columnGroups = list(
            reactable::colGroup(
              name = interface_translator$t(selected_language[35]),
              columns = c("Lower90", "Upper90")
              #columns = c(interface_translator$t(selected_language[36]), interface_translator$t(selected_language[37]))
            )
          ),
          theme = reactable::reactableTheme(
            backgroundColor = "#153015"
          ),
        )
        
      })
      
    }
  )
}