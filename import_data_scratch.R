
language <- "English"

language <- tolower(language)

selected_questions <- list(
  Group_1 = list(
    binary = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19), 
    range = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  ), 
  
  Group_2 = list(
    binary = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), 
    range = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  ), 
  
  Group_3 = list(
    binary = c(21, 23, 25, 27, 29, 31, 33, 35, 37, 39), 
    range = c(21, 23, 25, 27, 29, 31, 33, 35, 37, 39)
  ), 
  
  Group_4 = list(
    binary = NULL, 
    range = c(40:59)
  ), 
  
  Group_5 = list(
    binary = NULL, 
    range = c(60:79)
  )
)

binary_questions <- selected_questions %>% 
  purrr::map(function(x) x$binary) %>% 
  tibble::enframe(
    name = "Group", 
    value = "QuestionNumber"
  ) %>% 
  tidyr::unnest(cols = c(QuestionNumber))

range_questions <- selected_questions %>% 
  purrr::map(function(x) x$range) %>% 
  tibble::enframe(
    name = "Group", 
    value = "QuestionNumber"
  ) %>% 
  tidyr::unnest(cols = c(QuestionNumber))


googlesheets4::gs4_deauth()

url <- "https://docs.google.com/spreadsheets/d/1yTboPXmDMF43YmjsuEH7bbPwcEj4fPfBD68rNWrOPSI/edit?usp=sharing"

sheets_cols <- c(
  "Number", 
  paste0("Question_", language), 
  "Answer", 
  "Source_link"
) 

binary_questions_final <- googlesheets4::read_sheet(
  ss = url, 
  sheet = "Binary_questions"
) %>% 
  dplyr::select(dplyr::all_of(sheets_cols)) %>% 
  dplyr::right_join(
    binary_questions, 
    by = c("Number" = "QuestionNumber")
  ) %>% 
  dplyr::rename_with(~ stringr::str_remove(.x, paste0("_", language))) %>% 
  dplyr::rename(NumberGS = Number)

range_questions_final <- googlesheets4::read_sheet(
  ss = url, 
  sheet = "Range_questions"
) %>% 
  dplyr::select(dplyr::all_of(sheets_cols)) %>% 
  dplyr::right_join(
    range_questions, 
    by = c("Number" = "QuestionNumber")
  ) %>% 
  dplyr::rename_with(~ stringr::str_remove(.x, paste0("_", language))) %>% 
  dplyr::rename(NumberGS = Number)


