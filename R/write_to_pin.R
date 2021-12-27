


write_to_pin <- function(board, type, data, user_first, user_last) {
  
  pins::pin_write(
    board = board, 
    x = data %>% dplyr::mutate(User = glue::glue("{user_last}, {user_first}")), 
    name = glue::glue("{type}_{user_last}_{user_first}"), 
    type = "rds"
  )
  
}