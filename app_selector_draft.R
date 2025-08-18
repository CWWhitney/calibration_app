library(shiny)
library(rhandsontable)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  rHandsontableOutput("hot"),
  verbatimTextOutput("table_values")
)

server <- function(input, output, session) {
  values <- reactiveValues(
    data = data.frame(
      LogicalColumn1 = c(TRUE, FALSE, TRUE, FALSE, TRUE),
      LogicalColumn2 = c(FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    col_count = 2
  )
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data, rowHeaders = NULL) |>
      hot_col("LogicalColumn1", type = "checkbox", readOnly = TRUE) |> 
      hot_context_menu(
        allowRowEdit = FALSE, 
        allowColEdit = TRUE,
        customOpts = list(
          setTrue = list(
            name = "Set to TRUE",
            callback = htmlwidgets::JS(
              "function (key, options) {
                 var selected = this.getSelected();
                 console.log('Set to TRUE selected cells:', selected);
                 if (selected) {
                   for (var i = 0; i < selected.length; i++) {
                     var startRow = selected[i][0];
                     var endRow = selected[i][2];
                     var col = selected[i][1];
                     for (var row = startRow; row <= endRow; row++) {
                       this.setDataAtCell(row, col, true);
                     }
                   }
                 }
               }"
            )
          ),
          setFalse = list(
            name = "Set to FALSE",
            callback = htmlwidgets::JS(
              "function (key, options) {
                 var selected = this.getSelected();
                 console.log('Set to FALSE selected cells:', selected);
                 if (selected) {
                   for (var i = 0; i < selected.length; i++) {
                     var startRow = selected[i][0];
                     var endRow = selected[i][2];
                     var col = selected[i][1];
                     for (var row = startRow; row <= endRow; row++) {
                       this.setDataAtCell(row, col, false);
                     }
                   }
                 }
               }"
            )
          ),
          addColumn = list(
            name = "Add Column",
            callback = htmlwidgets::JS(
              "function (key, options) {
                 Shiny.setInputValue('addColumn', Math.random());
               }"
            )
          )
        )
      )
  })
  
  observeEvent(input$addColumn, {
    values$col_count <- values$col_count + 1
    new_col_name <- paste0("Group", values$col_count)
    values$data[[new_col_name]] <- rep(FALSE, nrow(values$data))
  })
  
  observe({
    if (!is.null(input$hot)) {
      values$data <- hot_to_r(input$hot)
    }
  })
  
  output$table_values <- renderPrint({
    values$data
  })
}

shinyApp(ui, server)
