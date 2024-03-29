


generate_binary_metrics_chart <- function(data) {
  
  data %>% 
    dplyr::mutate(
      Group = paste0("Group ", Group), 
      Truth = ifelse(Truth == "T", selected_language[31], selected_language[32]), 
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
      name = selected_language[38]
    ) |> 
    echarts4r::e_line(
      serie = Confidence, 
      name = selected_language[39], 
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



generate_range_metrics_chart <- function(data) {
  
  data %>% 
    dplyr::mutate(
      Group = paste0("Group ", Group), 
      Bounded = (Truth >= Lower90 & Truth <= Upper90) 
    ) %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Confidence = 0.9,   # goal of 90%  
      Correct = sum(Bounded) / dplyr::n(), 
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Group) |> 
    echarts4r::e_bar(
      serie = Correct, 
      name = "Korrekt"
    ) |> 
    echarts4r::e_line(
      serie = Confidence, 
      name = selected_language[40], 
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
