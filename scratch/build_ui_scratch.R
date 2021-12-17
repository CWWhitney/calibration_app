


source("import_data_scratch.R")

binary_questions_list <- binary_questions_final %>% 
  split(.$Group) %>% 
  purrr::map(~ dplyr::mutate(.x, QuestionNumber = dplyr::row_number()))

range_questions_list <- range_questions_final %>% 
  split(.$Group) %>% 
  purrr::map(~ dplyr::mutate(.x, QuestionNumber = dplyr::row_number()))

# Initialize two empty lists
binary_ui <- list()
current_group_ui <- list()

# Fill the nested lists
for (g in names(binary_questions_list)) {
  
  current_tbl <- binary_questions_list %>% 
    purrr::pluck(g)
  
  for (i in 1:(nrow(current_tbl))) {
    
    current_group_ui[[paste0("question_", i)]] <- shiny::tagList(
      
      shiny::h3(
        paste0("Question: ", i)
      ), 
      
      shiny::hr(), 
      
      shiny::h4(current_tbl$Question[i]), 
      
      shiny::br(), 
      
      shinyWidgets::awesomeRadio(
        inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_binary_answer_{i}_ui_A"),
        label = "Answer:",
        choices = c("TRUE", "FALSE"),
        selected = "TRUE",
        status = "warning"
      ), 
      shiny::sliderInput(
        inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_binary_answer_{i}_ui_B"), 
        label = "Confidence:", 
        min = 50, 
        max = 100, 
        value = 60, 
        step = 5, 
        post = "%"
      )
      
    )
    
  }
  
  binary_ui[[g]] <- current_group_ui
  
}

binary_ui


# Initialize two empty lists
range_ui <- list()
current_group_ui <- list()

# Fill the nested lists
for (g in names(range_questions_list)) {
  
  current_tbl <- range_questions_list %>% 
    purrr::pluck(g)
  
  for (i in 1:(nrow(current_tbl))) {
    
    current_group_ui[[paste0("question_", i)]] <- shiny::tagList(
      
      shiny::h3(
        paste0("Question: ", i)
      ), 
      
      shiny::hr(), 
      
      shiny::h4(current_tbl$Question[i]), 
      
      shiny::br(), 
      
      shiny::h5("90% Confidence Interval:"),
      
      shiny::div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_range_answer_{i}_ui_A"),
          label = "Lower Bound",
          value = 0
        )
      ), 
      
      shiny::div(
        style = "display: inline-block;",
        shiny::numericInput(
          inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_range_answer_{i}_ui_B"),
          label = "Upper Bound",
          value = 100
        )
      )
      
    )
    
  }
  
  range_ui[[g]] <- current_group_ui
  
}

range_ui



