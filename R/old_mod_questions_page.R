#' ##******************************************************************************
#' ##* 
#' ##* This is the questions_page shiny module which provides the UI for displaying
#' ##* questions and capturing user responses.
#' ##* 
#' ##******************************************************************************
#' 
#' #' Questions Page UI Module
#' #'
#' #' This module creates the UI for the questions page, including the question 
#' #' display, navigation buttons, and results panels.
#' #'
#' #' @param id The namespace id of the module.
#' #' @param tab_title The title of the tab panel.
#' #' @param next_btn_label The label for the next button.
#' #' @param binary_results_panel_title The title for the binary results panel.
#' #' @param range_results_panel_title The title for the range results panel.
#' #'
#' #' @return An HTML element for use in a UI.
#' #'
#' #' @seealso [mod_questions_page_server()]
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' if(interactive()){
#' #'   shiny::shinyApp(
#' #'     ui = shiny::fluidPage(
#' #'       mod_questions_page_ui(
#' #'         id = "mod_questions_page",
#' #'         tab_title = "Questions",
#' #'         next_btn_label = "Next",
#' #'         binary_results_panel_title = "Binary Results",
#' #'         range_results_panel_title = "Range Results"
#' #'       )
#' #'     ),
#' #'     server = function(input, output, session) {
#' #'       mod_questions_page_server(
#' #'         id = "mod_questions_page"
#' #'       )
#' #'     }
#' #'   )
#' #' }
#' mod_questions_page_ui <- function(id, tab_title, next_btn_label, binary_results_panel_title, range_results_panel_title) {
#'   ns <- shiny::NS(id)
#'   shiny::tabPanel(
#'     title = tab_title, 
#'     
#'     shiny::fluidRow(
#'       ### Questions UI Elements ------------------------------------------------
#'       shiny::column(
#'         width = 6, 
#'         shiny::wellPanel(
#'           style = "background: #153015;", 
#'           
#'           #### Question & Response UI -------------------------------------------
#'           shiny::uiOutput(outputId = ns("question_ui")), 
#'           
#'           shiny::hr(), 
#'           
#'           #### Previous / Next Buttons ------------------------------------------
#'           shiny::div(
#'             style = "float: right;",
#'             shiny::actionButton(
#'               class = "btn btn-lg", 
#'               inputId = ns("next_btn"), 
#'               label = next_btn_label, 
#'               icon = shiny::icon(name = "arrow-right")
#'             )
#'           ), 
#'           
#'           shiny::br(), 
#'           shiny::br()
#'         )
#'       ), 
#'       
#'       ### Response UI Elements -------------------------------------------------
#'       shiny::column(
#'         width = 6, 
#'         
#'         shiny::tabsetPanel(
#'           id = "results_tabset", 
#'           
#'           #### Binary Results Table --------------------------------------------
#'           shiny::tabPanel(
#'             title = binary_results_panel_title, 
#'             value = "binary_results_panel", 
#'             reactable::reactableOutput(outputId = ns("results_binary_tbl"))
#'           ), 
#'           
#'           #### Range Results Table ---------------------------------------------
#'           shiny::tabPanel(
#'             title = range_results_panel_title, 
#'             value = "range_results_panel", 
#'             reactable::reactableOutput(outputId = ns("results_range_tbl"))
#'           )
#'         )
#'       )
#'     )
#'   )
#' }
#' 
#' 
#' 
#' #' @param id The namespace id of the module.
#' #'
#' #' @return `list` of `shiny::reactive({})`s
#' #'
#' #' @seealso [questions_page_ui()]
#' #'
#' #' @export
#' #'
#' #' @inherit mod_questions_page_ui title description details examples
#' mod_questions_page_server <- function(
#'     id,
#'     current_question_type,
#'     current_group_number,
#'     current_question_number,
#'     question_index,
#'     rctv
#' ) {
#'   moduleServer(
#'     id,
#'     function(input, output, session) {
#'       ns <- session$ns
#'       
#'       ## Build Waiting Screen --------------------------------------------------
#'       w <- waiter::Waiter$new(
#'         id = c("question_ui"), 
#'         html = shiny::tagList(
#'           waiter::spin_flower(), 
#'           "Loading Next Question..."
#'         ), 
#'         color = "#153015",
#'       )
#'       
#'       output$question_ui <- shiny::renderUI({
#'         
#'         # Require the current question type, group number, and question number
#'         shiny::req(
#'           current_question_type(), 
#'           current_group_number(), 
#'           current_question_number()
#'         )
#'         
#'         question <- 
#'           questions |> 
#'           purrr::pluck(
#'             current_question_type()
#'           ) |> 
#'           filter(
#'             Group == stringr::str_c("Group_", current_group_number()) &
#'               QuestionNumber == current_question_number()
#'           ) |> 
#'           pull(
#'             Question
#'           )
#'         
#'         if (current_question_type() == "binary") {
#'           question <- binary_translator$t(question)
#'         } 
#'         
#'         if(current_question_type() == "range") {
#'           question <- range_translator$t(question)
#'         }
#'         
#'         # Display the appropriate UI response elements based on the current question type
#'         mod_question_ui(
#'           ns(
#'             stringr::str_glue(
#'               "group_{current_group_number()}_{current_question_type()}_answer_{current_question_number()}"
#'             )
#'           ),
#'           question = question,
#'           question_row_number = current_question_number(),
#'           type = current_question_type(),
#'           word_for_question = interface_translator$t(selected_language[44]),
#'           word_for_answer = interface_translator$t(selected_language[45]),
#'           word_for_correct = interface_translator$t(selected_language[31]),
#'           word_for_incorrect = interface_translator$t(selected_language[32]),
#'           word_for_confidence = interface_translator$t(selected_language[46]),
#'           word_for_confidence_interval = interface_translator$t(selected_language[35]),
#'           word_for_lower_bound = interface_translator$t(selected_language[33]),
#'           word_for_upper_bound = interface_translator$t(selected_language[34])
#'         )
#'         
#'         
#'       })
#'       
#'       question_responses <- reactiveValues(
#'         A = NULL, 
#'         B = NULL
#'       )
#'       
#'       
#'       ## Next Button -----------------------------------------------------------
#'       # When the "Next" button is clicked...
#'       shiny::observeEvent(input$next_btn, {
#'         #shinyjs::disable("next_btn")
#'         
#'         question_reactives <- 
#'           mod_question_server(
#'             stringr::str_glue(
#'               "group_{current_group_number()}_{current_question_type()}_answer_{current_question_number()}"
#'             )
#'           )
#'         
#'         question_responses$A <- question_reactives$A()
#'         question_responses$B <- question_reactives$B()
#'         
#'         # Capture the current response / Lower90
#'         current_response_1 <- question_responses$A 
#'         
#'         # Capture the current Confidence / Upper90 
#'         current_response_2 <-  question_responses$B
#'         
#'         # For "range" type questions, ensure that the entry for "Lower90" is less
#'         # than the value entered for "Upper90"
#'         if ((current_question_type() == "range") & (current_response_1 >= current_response_2)) {
#'           
#'           # Build a modal asking the user to fix the issue
#'           modal <- shiny::modalDialog(
#'             title = interface_translator$t(selected_language[15]),
#'             paste0(
#'               interface_translator$t(selected_language[16]), interface_translator$t(selected_language[17])
#'             ),
#'             footer = shiny::modalButton(
#'               label = interface_translator$t(selected_language[18]),
#'               icon = shiny::icon("pen")
#'             )
#'           )
#'           
#'         } else {
#'           
#'           # Create the first modal text segment
#'           modal_text_1 <- ifelse(
#'             current_question_type() == "binary", 
#'             interface_translator$t(selected_language[19]), 
#'             interface_translator$t(selected_language[20])
#'           )
#'           
#'           # Create the second modal text segment
#'           modal_text_2 <- ifelse(
#'             current_question_type() == "binary", 
#'             interface_translator$t(selected_language[21]), 
#'             interface_translator$t(selected_language[22])
#'           )
#'           
#'           # Create the modal text suffix
#'           modal_text_3 <- ifelse(
#'             current_question_type() == "binary", 
#'             "%", 
#'             ""
#'           )
#'           
#'           # Build a modal asking user to confirm their answer
#'           modal <- shiny::modalDialog(
#'             title = interface_translator$t(selected_language[23]), 
#'             glue::glue("{modal_text_1} {current_response_1}"), 
#'             shiny::br(), 
#'             glue::glue("{modal_text_2} {current_response_2}{modal_text_3}"), 
#'             easyClose = FALSE, 
#'             footer = shiny::tagList(
#'               shiny::div(
#'                 # Button to dismiss the modal
#'                 shiny::modalButton(
#'                   label = interface_translator$t(selected_language[18]), 
#'                   icon = shiny::icon("pen")
#'                 ), 
#'                 # Button to move to the next question
#'                 shiny::actionButton(
#'                   inputId = ns("submit_answer_btn"), 
#'                   label = interface_translator$t(selected_language[14]), 
#'                   icon = shiny::icon("check")
#'                 )
#'               )
#'             )
#'           )
#'           
#'         }
#'         
#'         # Launch the modal pop-up
#'         shiny::showModal(modal)
#'         #shinyjs::enable("next_btn")
#'       })
#'       
#'       
#'       ## Submit Answer Button --------------------------------------------------
#'       # When the "Submit" button is clicked...
#'       shiny::observeEvent(input$submit_answer_btn, {
#'         
#'         # ... remove the open modal dialogue
#'         shiny::removeModal()
#'         
#'         # Capture the current response / Lower90
#'         current_response_1 <- question_responses$A 
#'         
#'         # Capture the current Confidence / Upper90 
#'         current_response_2 <-  question_responses$B
#'         
#'         # Show the waiting screen
#'         w$show()
#'         
#'         # Append a new row to the reactive "binary" or "range" data frame 
#'         if (current_question_type() == "binary") {
#'           
#'           current_question <- questions |> 
#'             purrr::pluck(
#'               current_question_type()
#'             ) |> 
#'             filter(
#'               Group == stringr::str_c("Group_", current_group_number()) &
#'                 QuestionNumber == current_question_number()
#'             )
#'           
#'           
#'           rctv$binary_tbl <- rctv$binary_tbl |>
#'             rbind(
#'               data.frame(
#'                 Group = current_group_number(), 
#'                 Question = current_question$QuestionNumber, 
#'                 QuestionText = current_question$Question, 
#'                 Index = current_question_number(), 
#'                 Response = current_response_1, 
#'                 Confidence = paste0(current_response_2, "%"), 
#'                 Truth = current_question$Answer, 
#'                 Brier = brier(
#'                   #response = stringr::str_sub(current_response_1, 1L, 1L),
#'                   response = ifelse(current_response_1 == interface_translator$t(selected_language[31]), "T", "F"),
#'                   confidence = (current_response_2 / 100), 
#'                   correct_answer = current_question$Answer
#'                 ), 
#'                 Source = current_question$Source_link, 
#'                 stringsAsFactors = FALSE
#'               )
#'             )
#'           
#'           
#'           
#'         } else {
#'           
#'           current_question <- question_index |> 
#'             dplyr::filter(Index == current_question_number()) |> 
#'             dplyr::select(Group, QuestionNumber) |> 
#'             dplyr::mutate(Group = paste0("Group_", Group)) |> 
#'             dplyr::inner_join(
#'               questions$range, 
#'               by = c("Group", "QuestionNumber")
#'             )
#'           
#'           rctv$range_tbl <- rctv$range_tbl |>
#'             rbind(
#'               data.frame(
#'                 Group = current_group_number(), 
#'                 Question = current_question$QuestionNumber, 
#'                 QuestionText = current_question$Question, 
#'                 Index = current_question_number(), 
#'                 Lower90 = current_response_1, 
#'                 Upper90 = current_response_2, 
#'                 Truth = current_question$Answer, 
#'                 RelativeError = relative_error(
#'                   lower_90 = current_response_1,
#'                   upper_90 = current_response_2,
#'                   correct_answer = current_question$Answer
#'                 ), 
#'                 Source = current_question$Source_link, 
#'                 stringsAsFactors = FALSE
#'               )
#'             )
#'           
#'         }
#'         
#'         # If the submission was the last question in the *entire* workshop...
#'         if (current_question_number() == max(question_index$Index)) {
#'           
#'           # Write out the current results to the user's pin
#'           write_to_pin(
#'             board = board, 
#'             type = "binary", 
#'             data = rctv$binary_tbl, 
#'             user_first = trimws(input$user_first_name), 
#'             user_last = trimws(input$user_last_name)
#'           )
#'           
#'           write_to_pin(
#'             board = board, 
#'             type = "range", 
#'             data = rctv$range_tbl, 
#'             user_first = trimws(input$user_first_name), 
#'             user_last = trimws(input$user_last_name)
#'           )
#'           
#'           # Hide the {waiter} loading screen
#'           w$hide()
#'           
#'           # Remove the "question_ui" output element (to keep it from overlapping 
#'           # the pop-up modal we create next)
#'           shiny::removeUI(
#'             selector = "div:has(> #question_ui)", 
#'             immediate = TRUE
#'           )
#'           
#'           # Launch a pop-up modal letting the user know they have completed the 
#'           # workshop
#'           shiny::modalDialog(
#'             title = interface_translator$t(selected_language[24]), 
#'             glue::glue(
#'               interface_translator$t(selected_language[25]), 
#'             ), 
#'             shiny::br(), 
#'             interface_translator$t(selected_language[26]), 
#'             size = "l"
#'           ) |> 
#'             shiny::showModal()
#'           
#'         } else {
#'           
#'           # Increase the 'current_question_number' value by 1
#'           rctv$current_question_number <- current_question_number() + 1
#'           
#'           # Get the corresponding group number for the next question
#'           rctv$current_group_number <- question_index$Group[rctv$current_question_number]
#'           
#'           # Get the corresponding question type for the next question
#'           rctv$current_question_type <- question_index$QuestionType[rctv$current_question_number]
#'           
#'           
#'           # If the new question switches from "binary" to "range" (or vice versa), 
#'           # change the "Tables" tab to show the current table
#'           if (question_index$QuestionType[rctv$current_question_number] != question_index$QuestionType[rctv$current_question_number - 1]) {
#'             
#'             shiny::updateTabsetPanel(
#'               session = session, 
#'               inputId = "results_tabset", 
#'               selected = paste0(rctv$current_question_type, "_results_panel")
#'             )
#'             
#'           }
#'           
#'           # If the new question begins a new group, write the most current results to
#'           # {pins} database and show a pop-up
#'           if (question_index$Group[rctv$current_question_number] != question_index$Group[rctv$current_question_number - 1]) {
#'             
#'             
#'             rctv$binary_tbl_backend <- rctv$binary_tbl |> 
#'               rename_at( 2, ~"Question") |>
#'               rename_at( 5, ~"Response") |>
#'               rename_at( 6, ~"Confidence") |>
#'               rename_at( 7, ~"Truth")
#'             
#'             
#'             write_to_pin(
#'               board = board, 
#'               type = "binary", 
#'               data = rctv$binary_tbl_backend, 
#'               user_first = trimws(input$user_first_name), 
#'               user_last = trimws(input$user_last_name)
#'             )
#'             
#'             rctv$range_tbl_backend <- rctv$range_tbl |> 
#'               rename_at( 2, ~"Question") |>
#'               rename_at( 5, ~"Lower90") |>
#'               rename_at( 6, ~"Upper90") |>
#'               rename_at( 7, ~"Truth")
#'             
#'             write_to_pin(
#'               board = board, 
#'               type = "range", 
#'               data = rctv$range_tbl_backend, 
#'               user_first = trimws(input$user_first_name), 
#'               user_last = trimws(input$user_last_name)
#'             )
#'             
#'             # Show a "Group Complete" pop-up modal
#'             shiny::modalDialog(
#'               title = interface_translator$t(selected_language[27]), 
#'               glue::glue(
#'                 interface_translator$t(selected_language[28]), 
#'               ), 
#'               footer = modalButton(interface_translator$t(selected_language[29])),
#'               shiny::br(), 
#'               interface_translator$t(selected_language[30]), 
#'               size = "l"
#'             )|> 
#'               shiny::showModal()
#'             
#'           }
#'           
#'         }
#'         
#'       })
#'       
#'       
#'       
#'     }
#'   )
#' }
#' 
#' 
#' 
#' 
#' ## Binary Results Table ----
#' # Create the table to hold the "Binary" results & scores
#' output$results_binary_tbl <- reactable::renderReactable({
#'   
#'   # Require the "binary" response table
#'   shiny::req(rctv$binary_tbl)
#'   
#'   data <- rctv$binary_tbl |> 
#'     dplyr::filter(Group == rctv$current_group_number)
#'   
#'   #colnames(rctv$binary_tbl)[colnames(rctv$binary_tbl) == "Question"] = "interface_translator$t(selected_language[42])"
#'   
#'   # Populate the interactive table with the "binary" data from the current 
#'   # question group
#'   reactable::reactable(
#'     data, 
#'     columns = list(
#'       Question = reactable::colDef(cell = function(value, index) {
#'         hover <- data[index, "QuestionText"]
#'         # Render as text that can be hovered over to show full question
#'         htmltools::tags$span(
#'           title = hover,
#'           value
#'         )
#'       }, name = interface_translator$t(selected_language[42])),
#'       Group = reactable::colDef(show = FALSE), 
#'       Index = reactable::colDef(show = FALSE), 
#'       Response = reactable::colDef(show = TRUE, name = interface_translator$t(selected_language[41])),
#'       Confidence = reactable::colDef(show = TRUE, name = interface_translator$t(selected_language[40])),
#'       Brier = reactable::colDef(
#'         format = reactable::colFormat(digits = 2)
#'       ), 
#'       Truth = reactable::colDef(
#'         cell = function(value, index) {
#'           text <- if (value == "T") interface_translator$t(selected_language[31]) else interface_translator$t(selected_language[32])
#'           url <- data[index, "Source"]
#'           # Render as a link
#'           htmltools::tags$a(
#'             href = url, 
#'             target = "_blank", 
#'             text
#'           )
#'         }, name = interface_translator$t(selected_language[43])), 
#'       QuestionText = reactable::colDef(show = FALSE), 
#'       Source = reactable::colDef(show = FALSE)
#'     ), 
#'     theme = reactable::reactableTheme(
#'       backgroundColor = "#153015"
#'     )
#'   )
#' })
#' 
#' 
#' ## Results Table ----
#' # Create the table to hold the "Range" results & scores
#' output$results_range_tbl <- reactable::renderReactable({
#'   
#'   # Require the "range" response table
#'   shiny::req(rctv$range_tbl)
#'   
#'   data <- rctv$range_tbl |> 
#'     dplyr::filter(Group == rctv$current_group_number)
#'   
#'   # Populate the interactive table with the "range" data from the current 
#'   # question group
#'   reactable::reactable(
#'     data, 
#'     columns = list(
#'       Question = reactable::colDef(cell = function(value, index) {
#'         hover <- data[index, "QuestionText"]
#'         # Render as text that can be hovered over to show full question
#'         htmltools::tags$span(
#'           title = hover,
#'           value
#'         )
#'       }, name = interface_translator$t(selected_language[42])),
#'       Group = reactable::colDef(show = FALSE),
#'       Index = reactable::colDef(show = FALSE),
#'       Lower90 = reactable::colDef(name = interface_translator$t(selected_language[33])),
#'       Upper90 = reactable::colDef(name = interface_translator$t(selected_language[34])),
#'       RelativeError = reactable::colDef(
#'         name = interface_translator$t(selected_language[47]),
#'         format = reactable::colFormat(digits = 2)
#'       ),
#'       Truth = reactable::colDef(
#'         cell = function(value, index) {
#'           url <- data[index, "Source"] 
#'           
#'           if(is.na(url)){
#'             htmltools::p(value)
#'           } else {
#'             # hover <- data[index, "Comments"]
#'             # Render as a link
#'             htmltools::tags$a(
#'               # title = hover,
#'               href = url,
#'               target = "_blank",
#'               value
#'             )
#'           }
#'         }, name = interface_translator$t(selected_language[48])),
#'       QuestionText = reactable::colDef(show = FALSE),
#'       Source = reactable::colDef(show = FALSE)
#'     ), 
#'     columnGroups = list(
#'       reactable::colGroup(
#'         name = interface_translator$t(selected_language[35]), 
#'         columns = c("Lower90", "Upper90")
#'         #columns = c(interface_translator$t(selected_language[36]), interface_translator$t(selected_language[37]))
#'       )
#'     ), 
#'     theme = reactable::reactableTheme(
#'       backgroundColor = "#153015"
#'     ),
#'   )
#'   
#' })
