## UI
ui <- function() {
  div(
    shiny.i18n::usei18n(interface_translator),
    bslib::page_navbar(
      # Set Up Global UI Elements ----------------------------------------------
      title = interface_translator$t(selected_language[1]),
      theme = app_theme,
      navbar_options = navbar_options(collapsible = TRUE),
      ## Ensure tickmark text on "Confidence" sliders is white  
      shiny::tags$head(
        shiny::tags$link(
          rel = "stylesheet", 
          type = "text/css", 
          href = "style.css"
        ),
        tags$script(
          HTML("window.onbeforeunload = function(evt) {return true;}")
        )
      ), 
      # nav_spacer(),
      # ## Language Selection ------------------------------------------------
      # nav_item(
      #   uiOutput("language_selection_ui"),
      # ),
      nav_spacer(),
      # Questions Page -------------------------------------------------------
      mod_questions_page_ui(
        id = "questions_page",
        tab_title = interface_translator$t(selected_language[2]),
        binary_results_panel_title = interface_translator$t(selected_language[4]),
        range_results_panel_title = interface_translator$t(selected_language[5])
      ), 
      
      # Metrics Page ---------------------------------------------------------
      mod_metrics_page_ui(
        id = "metrics_page",
        tab_title = interface_translator$t(selected_language[6]),
        left_column_header = interface_translator$t(selected_language[7]),
        right_column_header = interface_translator$t(selected_language[8]),
        text_center = interface_translator$t(selected_language[9])
      ), 
      # Help Pages ------------------------------------------------------------
      mod_help_page_ui(
        id = "help_page",
        tab_title =  interface_translator$t(selected_language[10]),
        url = c(
          "'https://www.youtube.com/embed/QPdJNqXHxmI'",
          "'https://www.youtube.com/embed/OtYAomR9pZE?si=k8jtETxukBYJvWE0'",
          "'https://www.youtube.com/embed/3YeWSHCUh9w?si=Jb7A8CjZZkyG0l0X'",
          "'https://www.youtube.com/embed/eKvCAZd7px8?si=y9eucgVBZXa32Ag7'",
          "'https://www.youtube.com/embed/qwHvGh_9tRs?si=Mit1VkZ544EgMF3C'"
        )
      ),
      footer = tags$footer(
        class = "d-flex justify-content-end pb-3",
        uiOutput("language_selection_ui")
      )
      
    )
  )
}