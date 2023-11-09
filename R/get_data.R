


get_data <- function(selected_questions_list, language, gs_url) {
  
  language <- tolower(language)
  
 
  
  # Convert range questions from list to data frame for downstream join
  range_question_numbers <- selected_questions %>% 
    purrr::map(function(x) x$range) %>% 
    tibble::enframe(
      name = "Group", 
      value = "QuestionNumber"
    ) %>% 
    tidyr::unnest(cols = c(QuestionNumber))
  
  # Define the columns we want to bring in from the Google Sheet
  sheets_cols <- c(
    "Number", 
    paste0("Question_", language), 
    "Answer", 
    "Source_link", 
    "Comments"
  )
  
  
  
  # Read in the "Range" questions from the Google Sheet, keeping only the 
  # desired columns & question numbers
  range_questions <- googlesheets4::read_sheet(
    ss = gs_url, 
    sheet = "Range_questions"
  ) %>% 
    dplyr::select(dplyr::all_of(sheets_cols)) %>% 
    dplyr::right_join(
      range_question_numbers, 
      by = c("Number" = "QuestionNumber")
    ) %>% 
    dplyr::rename_with(~ stringr::str_remove(.x, paste0("_", language))) %>% 
    dplyr::rename(NumberGS = Number) %>% 
    tidyr::unnest(cols = c(Source_link, Comments)) %>% 
    dplyr::group_by(Group) %>% 
    dplyr::mutate(QuestionNumber = dplyr::row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(Group, QuestionNumber)
  
  # Return the two data frames as a list
  list(
    #binary = binary_questions, 
    range = range_questions
  )
  
}
