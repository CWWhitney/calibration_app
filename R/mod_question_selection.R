# Define UI for the module
mod_question_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             tags$h3("Group 1"),
             shinyWidgets::pickerInput(
               inputId = ns("group_1_binary"),
               label = "Binary",
               choices = questions_full$binary$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 138:147,
               multiple = TRUE
             ),
             shinyWidgets::pickerInput(
               inputId = ns("group_1_range"),
               label = "Range",
               choices = questions_full$range$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 153:162,
               multiple = TRUE
             )
      ),
      column(2,
             tags$h3("Group 2"),
             shinyWidgets::pickerInput(
               inputId = ns("group_2_binary"),
               label = "Binary",
               choices = questions_full$binary$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = c(148:151, 153),
               multiple = TRUE
             ),
             shinyWidgets::pickerInput(
               inputId = ns("group_2_range"),
               label = "Range",
               choices = questions_full$range$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = c(175:182, 184, 207),
               multiple = TRUE
             )
      ),
      column(2,
             tags$h3("Group 3"),
             shinyWidgets::pickerInput(
               inputId = ns("group_4_binary"),
               label = "Binary",
               choices = questions_full$binary$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = NULL,
               multiple = TRUE
             ),
             shinyWidgets::pickerInput(
               inputId = ns("group_3_range"),
               label = "Range",
               choices = questions_full$range$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = c(164, 165, 167:174),
               multiple = TRUE
             )
      ),
      column(2,
             tags$h3("Group 4"),
             shinyWidgets::pickerInput(
               inputId = ns("group_4_binary"),
               label = "Binary",
               choices = questions_full$binary$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 154:163,
               multiple = TRUE
             ),
             shinyWidgets::pickerInput(
               inputId = ns("group_4_range"),
               label = "Range",
               choices = questions_full$range$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 208:227,
               multiple = TRUE
             )
      ),
      column(2,
             tags$h3("Group 5"),
             shinyWidgets::pickerInput(
               inputId = ns("group_5_binary"),
               label = "Binary",
               choices = questions_full$binary$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 164:173,
               multiple = TRUE
             ),
             shinyWidgets::pickerInput(
               inputId = ns("group_5_range"),
               label = "Range",
               choices = questions_full$range$Number,
               options = shinyWidgets::pickerOptions(
                 container = "body",
                 liveSearch = TRUE
               ),
               selected = 242:261,
               multiple = TRUE
             )
      )
    ),
    verbatimTextOutput(ns("selected_questions_string")),
    reactable::reactableOutput(ns("decrpyted_questions_index"))
  )
}

# Define server logic for the module
mod_question_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic can be added here if needed
    
    selected_questions <- 
      reactive({
        list(
          Group_1 = list(
            binary = input$group_1_binary,
            range = input$group_1_range
          ),
          Group_2 = list(
            binary = input$group_2_binary,
            range = input$group_2_range
          ),
          Group_3 = list(
            binary = input$group_3_binary,
            range = input$group_3_range
          ),
          Group_4 = list(
            binary = input$group_4_binary,
            range = input$group_4_range
          ),
          Group_5 = list(
            binary = input$group_5_binary,
            range = input$group_5_range
          )
        )
      })
    
    questions <- reactive({
      selected_data(
        selected_questions_list = selected_questions(), 
        questions_full = questions_full,
        languages = languages
      )
    })
    
    
    question_index <- reactive({
      questions() |> 
        purrr::map_dfr(
          ~ dplyr::select(.x, NumberGS, Group, QuestionNumber), 
          .id = "QuestionType"
        ) |> 
        dplyr::mutate(Group = as.integer(stringr::str_sub(Group, -1, -1))) |> 
        dplyr::arrange(Group, QuestionType, QuestionNumber) |> 
        dplyr::mutate(Index = dplyr::row_number())
    })
    
    encrypted_question_index <- reactive({
      question_index() |> 
        dplyr::mutate(QuestionTypeCode = dplyr::if_else(QuestionType == "binary", 0, 1)) |> 
        dplyr::select(Group, QuestionTypeCode, NumberGS) |> 
        encrypt_question_index(key = our_key, nonce = our_nonce)
    })
    
    output$selected_questions_string <- renderPrint({
      encrypted_question_index()
    })
    
    output$decrpyted_questions_index <- reactable::renderReactable({
      reactable::reactable(
        encrypted_question_index() |> 
          decrypt_question_index(key = our_key, nonce = our_nonce),
        theme = reactable::reactableTheme(
          backgroundColor = "#153015"
        )
      )
    })
    
    
    return(encrypted_question_index)
  })
}
