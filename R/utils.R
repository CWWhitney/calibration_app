#' Calculate Relative Error of an Estimate
#'
#' Computes the relative error of a prediction interval given the lower and upper bounds
#' of a 90% confidence interval and the correct answer.
#'
#' @param lower_90 Numeric. The lower bound of the 90% confidence interval.
#' @param upper_90 Numeric. The upper bound of the 90% confidence interval.
#' @param correct_answer Numeric. The actual correct value.
#'
#' @return A numeric value representing the relative error.
#' @examples
#' relative_error(40, 60, 55)
#' @export
relative_error <- function(lower_90, upper_90, correct_answer) {
  midpoint <- (lower_90 + upper_90) / 2
  interval_width <- upper_90 - lower_90
  
  (correct_answer - midpoint) / interval_width * 2
}

#' Compute Brier Score for a Prediction
#'
#' Calculates the Brier score, which measures the accuracy of probabilistic predictions.
#'
#' @param response The predicted answer (can be logical, numeric, or character).
#' @param confidence Numeric. The confidence level of the prediction (between 0 and 1).
#' @param correct_answer The actual correct answer (same type as `response`).
#'
#' @return A numeric value representing the Brier score.
#' @examples
#' brier("yes", 0.8, "yes")
#' brier(TRUE, 0.6, FALSE)
#' @export
brier <- function(response, confidence, correct_answer) {
  indicator <- ifelse(response == correct_answer, 1, 0)
  (indicator - confidence)^2
}
