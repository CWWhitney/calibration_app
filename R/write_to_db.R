


write_to_db <- function(question_type, user, question_number, 
                        answer_1, answer_2) {
  
  if (question_type == "binary") {
    
    answer_info <- data.frame(
      UserID = user, 
      QuestionNumber = question_number, 
      UserAnswer = answer_1, 
      UserConfidence = answer_2
    )
    
  }
  
  if (question_type == "range") {
    
    answer_info <- data.frame(
      UserID = user, 
      QuestionNumber = question_number, 
      UserAnswerLower90 = answer_1, 
      UserAnswerUpper90 = answer_2
    )
    
  }
  
  write.table(
    x = answer_info, 
    file = glue::glue("data/responses/example_responses_db_{question_type}.csv"), 
    sep = ",", 
    append = TRUE, 
    row.names = FALSE, 
    col.names = FALSE
  )
  
}
