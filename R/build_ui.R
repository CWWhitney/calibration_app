


build_ui <- function(questions, type) {
  
  # Split the 'questions' data frame into a list of data frames, by "Group"
  questions_list <- questions %>% 
    split(.$Group) %>% 
    purrr::map(~ dplyr::mutate(.x, QuestionNumber = dplyr::row_number()))
  
  # Initialize two empty lists
  ui <- list()
  current_group_ui <- list()
  
  for (g in names(questions_list)) {
    
    current_tbl <- questions_list %>% 
      purrr::pluck(g)
    
    for (i in 1:(nrow(current_tbl))) {
      
      current_group_ui[[paste0("question_", i)]] <- shiny::tagList(
        
        shiny::h3(
          paste0("Question: ", i)
        ), 
        
        shiny::hr(), 
        
        shiny::h4(current_tbl$Question[i]), 
        
        shiny::br(), 
        

          shiny::tagList(
            shiny::div(
              style = "padding-left: 20px;", 
              shinyWidgets::awesomeRadio(
                inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_binary_answer_{i}_ui_A"),
                label = "Answer:",
                choices = c("TRUE", "FALSE"),
                selected = "TRUE",
                status = "warning"
              )
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
          
      )
      
    }
    
    ui[[g]] <- current_group_ui
    
  }
  
  return(ui)
  
}
