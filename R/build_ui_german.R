


build_ui_german <- function(questions, type) {
  
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
          paste0(selected_language[44], i)
        ), 
        
        shiny::hr(), 
        
        shiny::h4(current_tbl$Question[i]), 
        
        shiny::br(), 
        
        if (type == "binary") {
          
          shiny::tagList(
            shiny::div(
              style = "padding-left: 20px;", 
              shinyWidgets::awesomeRadio(
                inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_binary_answer_{i}_ui_A"),
                label = selected_language[45],
                choices = c(selected_language[31], selected_language[32]),
                selected = selected_language[31],
                status = "warning"
              )
            ), 
            
            shiny::sliderInput(
              inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_binary_answer_{i}_ui_B"), 
              label = selected_language[46], 
              min = 50, 
              max = 100, 
              value = 60, 
              step = 5, 
              post = "%"
            )
          )
          
        } else {
          
          shiny::tagList(
            shiny::h5(selected_language[35]),
            
            shiny::div(
              style = "display: inline-block;",
              shiny::numericInput(
                inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_range_answer_{i}_ui_A"),
                label = selected_language[33],
                value = 0
              )
            ), 
            
            shiny::div(
              style = "display: inline-block;",
              shiny::numericInput(
                inputId = glue::glue("group_{stringr::str_sub(g, -1L, -1L)}_range_answer_{i}_ui_B"),
                label = selected_language[34],
                value = 0
              )
            )
          )
          
        }
        
      )
      
    }
    
    ui[[g]] <- current_group_ui
    
  }
  
  return(ui)
  
}
