
brier <- function(response, confidence, correct_answer) {
  
  indicator <- ifelse(response == correct_answer, 1, 0)
  
  (indicator - confidence) ^ 2
  
}