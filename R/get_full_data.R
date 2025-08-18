get_full_data <- function(gs_url) {
  
  # Read in the "Binary" questions from the Google Sheet
  binary_questions <- 
  #   googlesheets4::read_sheet(
  #   ss = gs_url, 
  #   sheet = "Binary_questions"
  # ) |> 
    readRDS("data/Binary_questions.RDS") |> 
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
    
    
  # Read in the "Range" questions from the Google Sheet
  range_questions <- 
  #   googlesheets4::read_sheet(
  #   ss = gs_url, 
  #   sheet = "Range_questions"
  # ) |>  
    readRDS("data/Range_questions.RDS") |> 
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
  
  # Return the two data frames as a list
  list(
    binary = binary_questions, 
    range = range_questions
  )
  
}


selected_data <- function(
    selected_questions_list,
    questions_full,
    languages
    ) {
  
  
  # Convert binary questions from list to data frame for downstream join
  binary_question_numbers <- selected_questions_list |> 
    purrr::map(purrr::pluck("binary")) |> 
    tibble::enframe(
      name = "Group", 
      value = "QuestionNumber"
    ) |> 
    tidyr::unnest(cols = c(QuestionNumber)) |> 
    dplyr::mutate(QuestionNumber = as.double(QuestionNumber))
  
  # Convert range questions from list to data frame for downstream join
  range_question_numbers <- selected_questions_list |> 
    purrr::map(purrr::pluck("range")) |> 
    tibble::enframe(
      name = "Group", 
      value = "QuestionNumber"
    ) |> 
    tidyr::unnest(cols = c(QuestionNumber))|> 
    dplyr::mutate(QuestionNumber = as.double(QuestionNumber))
  
  # Define the columns we want to bring in from the Google Sheet
  sheets_cols <- c(
    "Number", 
    languages,
    "Answer", 
    "Source_link", 
    "Comments"
  )
  
  # Read in the "Binary" questions from the Google Sheet, keeping only the 
  # desired columns & question numbers
  binary_questions <- questions_full$binary |> 
    dplyr::select(dplyr::all_of(sheets_cols)) |> 
    dplyr::right_join(
      binary_question_numbers, 
      by = c("Number" = "QuestionNumber")
    ) |> 
    dplyr::rename_with(~ stringr::str_remove(.x, paste0("_", language))) |> 
    dplyr::rename(NumberGS = Number) |> 
    tidyr::unnest(cols = c(Source_link, Comments)) |> 
    dplyr::group_by(Group) |> 
    dplyr::mutate(QuestionNumber = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(Group, QuestionNumber)
  
  # Read in the "Range" questions from the Google Sheet, keeping only the 
  # desired columns & question numbers
  range_questions <- questions_full$range |> 
    dplyr::select(dplyr::all_of(sheets_cols)) |> 
    dplyr::right_join(
      range_question_numbers, 
      by = c("Number" = "QuestionNumber")
    ) |> 
    dplyr::rename_with(~ stringr::str_remove(.x, paste0("_", language))) |> 
    dplyr::rename(NumberGS = Number) |> 
    tidyr::unnest(cols = c(Source_link, Comments)) |> 
    dplyr::group_by(Group) |> 
    dplyr::mutate(QuestionNumber = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(Group, QuestionNumber)
  
  # Return the two data frames as a list
  list(
    binary = binary_questions, 
    range = range_questions
  )
  
}