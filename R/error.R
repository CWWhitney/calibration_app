

relative_error <- function(lower_90, upper_90, correct_answer) {
  
  
  ((correct_answer - (upper_90 + lower_90) / 2)) / (upper_90 - lower_90) * 2
  
}

