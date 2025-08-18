#' Generate Binary Metrics Chart
#'
#' Creates a chart comparing predicted correctness and average confidence across groups
#' for binary classification tasks.
#'
#' @param data A data frame containing columns: `Group`, `Truth`, `Response`, and `Confidence`.
#' @param label_true Character. Label to display when `Truth` is "T".
#' @param label_false Character. Label to display when `Truth` is not "T".
#' @param word_predicted Character. Label for the predicted correctness line.
#' @param word_correct Character. Label for the average confidence line.
#' @param word_for_round Character. Prefix for the round/group label.
#'
#' @return An `echarts4r` chart object.
#' @examples
#' generate_binary_metrics_chart(data, "Yes", "No", "Predicted", "Confidence", "Round")
#' @export
generate_binary_metrics_chart <- function(
    data,
    label_true,
    label_false,
    word_predicted,
    word_correct,
    word_for_round
) {
  data |>
    dplyr::mutate(
      Round = paste(word_for_round, Group),
      Truth = dplyr::if_else(Truth == "T", label_true, label_false),
      Truth2 = Truth == label_true,
      Confidence = stringr::str_remove(Confidence, "%") |> as.numeric()
    ) |>
    dplyr::group_by(Round) |>
    dplyr::summarise(
      Confidence = mean(Confidence / 100),
      Correct = mean(Response == Truth2),
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Round) |>
    echarts4r::e_bar(serie = Correct, name = word_predicted) |>
    echarts4r::e_line(serie = Confidence, name = word_correct, symbol = "circle", symbolSize = 20) |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent", digits = 0)) |>
    echarts4r::e_color(background = "white") |>
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent", digits = 1)
    ) |>
    echarts4r::e_toolbox_feature(feature = "saveAsImage")
}


#' Generate Range Metrics Chart
#'
#' Creates a chart comparing the proportion of correct range predictions to a fixed confidence level.
#'
#' @param data A data frame containing columns: `Group`, `Truth`, `Lower90`, and `Upper90`.
#' @param word_correct Character. Label for the proportion of correct predictions.
#' @param word_confidence Character. Label for the fixed confidence line.
#' @param word_for_round Character. Prefix for the round/group label.
#'
#' @return An `echarts4r` chart object.
#' @examples
#' generate_range_metrics_chart(data, "Correct", "Expected Confidence", "Round")
#' @export
generate_range_metrics_chart <- function(
    data,
    word_correct,
    word_confidence,
    word_for_round
) {
  data |>
    dplyr::mutate(
      Round = paste(word_for_round, Group),
      Bounded = Truth >= Lower90 & Truth <= Upper90
    ) |>
    dplyr::group_by(Round) |>
    dplyr::summarise(
      Confidence = 0.9,
      Correct = mean(Bounded),
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Round) |>
    echarts4r::e_bar(serie = Correct, name = word_correct) |>
    echarts4r::e_line(serie = Confidence, name = word_confidence, symbol = "circle", symbolSize = 20) |>
    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent", digits = 0)) |>
    echarts4r::e_color(background = "white") |>
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = echarts4r::e_tooltip_pointer_formatter(style = "percent", digits = 1)
    ) |>
    echarts4r::e_toolbox_feature(feature = "saveAsImage")
}

