


generate_binary_metrics_chart <- function(data) {
  
  data %>% 
    dplyr::mutate(
      Group = paste0("Group ", Group), 
      Loesung = ifelse(Loesung == "T", "Richtig", "Falsch"), 
      Konfidenz = stringr::str_replace(
        string = Konfidenz, 
        pattern = "%", 
        replacement = ""
      ) %>% as.numeric()
    ) %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Konfidenz = sum(Konfidenz / 100) / dplyr::n(), 
      Correct = sum(Antwort == Loesung) / dplyr::n(), 
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Group) |> 
    echarts4r::e_bar(
      serie = Correct, 
      name = "Korrekt"
    ) |> 
    echarts4r::e_line(
      serie = Konfidenz, 
      name = "Vorhergesagt", 
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
      Bounded = (korr.Antwort >= Untere90 & korr.Antwort <= Obere90) 
    ) %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Konfidenz = 0.9,   # goal of 90%  
      Correct = sum(Bounded) / dplyr::n(), 
      .groups = "drop"
    ) |>
    echarts4r::e_charts(x = Group) |> 
    echarts4r::e_bar(
      serie = Correct, 
      name = "Korrekt"
    ) |> 
    echarts4r::e_line(
      serie = Konfidenz, 
      name = "Konfidenz", 
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
