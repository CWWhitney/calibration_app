


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
                value = 0
              )
            )
          )
          
      )
      
    }
    
    ui[[g]] <- current_group_ui
    
  }
  
  return(ui)
  
}
