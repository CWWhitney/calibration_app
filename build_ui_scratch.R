

binary_ui <- binary_questions_final %>% 
  split(.$Group) %>% 
  purrr::map(~ dplyr::mutate(.x, QuestionNumber = dplyr::row_number()))


group_1_ui <- list()

for (i in 1:(nrow(binary_questions_final %>% dplyr::filter(Group == "Group_1")))) {
  
  group_1_ui[[i]] <- shiny::tagList(
    
    shiny::h3(
      paste0("Question: ", binary_ui$Group_1$QuestionNumber[i])
    ), 
    
    shiny::hr(), 
    
    shiny::h4(binary_ui$Group_1$Question[i]), 
    
    shiny::br(), 
    
    shinyWidgets::awesomeRadio(
      inputId = glue::glue("answer_ui_{i}_A"),
      label = "Answer:",
      choices = c("TRUE", "FALSE"),
      selected = "TRUE",
      status = "warning"
    ), 
    shiny::sliderInput(
      inputId = glue::glue("answer_ui_{i}_B"), 
      label = "Confidence:", 
      min = 50, 
      max = 100, 
      value = 60, 
      step = 5, 
      post = "%"
    )
    
  )
  
}
