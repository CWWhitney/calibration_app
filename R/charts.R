


generate_binary_metrics_chart <- function(data) {
  
  data %>% 
    dplyr::mutate(
      Group = paste0("Group ", Group), 
      Truth = ifelse(Truth == "T", "TRUE", "FALSE"), 
      Confidence = stringr::str_replace(
        string = Confidence, 
        pattern = "%", 
        replacement = ""
      ) %>% as.numeric()
    ) %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Confidence = sum(Confidence / 100) / dplyr::n(), 
      Correct = sum(Response == Truth) / dplyr::n(), 
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Group) |> 
    echarts4r::e_bar(
      serie = Correct, 
      name = "Correct"
    ) |> 
    echarts4r::e_line(
      serie = Confidence, 
      name = "Predicted", 
      symbol = "circle", 
      symbolSize = 20
    ) |> 
    echarts4r::e_y_axis(
      formatter = echarts4r::e_axis_formatter(
        style = "percent", 
        digits = 0
      )
    ) |> 
    echarts4r::e_color(
      background = "white"
    ) |> 
    echarts4r::e_tooltip(
      trigger = "axis", 
      formatter = echarts4r::e_tooltip_pointer_formatter(
        style = "percent", 
        digits = 1
      )
    ) |> 
    echarts4r::e_toolbox_feature(feature = "saveAsImage")
  
}


