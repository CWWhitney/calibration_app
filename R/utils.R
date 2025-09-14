get_full_data <- function(gs_url) {
  
  # Read in the "Binary" questions from the Google Sheet
  binary_questions <- 
    googlesheets4::read_sheet(
      ss = gs_url,
      sheet = "Binary_questions"
    ) |>
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
  
  
  # Read in the "Range" questions from the Google Sheet
  range_questions <- 
    googlesheets4::read_sheet(
      ss = gs_url,
      sheet = "Range_questions"
    ) |>
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_remove(.x, "Question_")), dplyr::starts_with("Question_"))
  
  # Return the two data frames as a list
  list(
    binary = binary_questions, 
    range = range_questions
  )
  
}

prepare_workshop_question_set <- function(
    questions_full,
    binary_rounds,
    range_rounds,
    languages
) {
  
  binary_question_numbers <- 
    binary_rounds |> 
    mutate(Group = stringr::str_c("Group_", Round), QuestionNumber = Number) |> 
    select(Group, QuestionNumber)
  
  range_question_numbers <- 
    range_rounds |> 
    mutate(Group = stringr::str_c("Group_", Round), QuestionNumber = Number) |> 
    select(Group, QuestionNumber)
  
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

#' Calculate Relative Error of an Estimate
#'
#' Computes the relative error of a prediction interval given the lower and upper bounds
#' of a 90% confidence interval and the correct answer.
#'
#' @param lower_90 Numeric. The lower bound of the 90% confidence interval.
#' @param upper_90 Numeric. The upper bound of the 90% confidence interval.
#' @param correct_answer Numeric. The actual correct value.
#'
#' @return A numeric value representing the relative error.
#' @examples
#' relative_error(40, 60, 55)
#' @export
relative_error <- function(lower_90, upper_90, correct_answer) {
  midpoint <- (lower_90 + upper_90) / 2
  interval_width <- upper_90 - lower_90
  
  (correct_answer - midpoint) / interval_width * 2
}

#' Compute Brier Score for a Prediction
#'
#' Calculates the Brier score, which measures the accuracy of probabilistic predictions.
#'
#' @param response The predicted answer (can be logical, numeric, or character).
#' @param confidence Numeric. The confidence level of the prediction (between 0 and 1).
#' @param correct_answer The actual correct answer (same type as `response`).
#'
#' @return A numeric value representing the Brier score.
#' @examples
#' brier("yes", 0.8, "yes")
#' brier(TRUE, 0.6, FALSE)
#' @export
brier <- function(response, confidence, correct_answer) {
  indicator <- ifelse(response == correct_answer, 1, 0)
  (indicator - confidence)^2
}


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




#' Initialize Shiny Application State
#'
#' Sets up global variables for tracking application state, including a unique
#' application token, start time, and active session counter. Also registers a
#' shutdown message when the app stops.
#'
#' @details
#' This function should be called once at the start of a Shiny application.
#' It initializes:
#' - `application_token`: a unique 8-character ID
#' - `application_start_time`: the system time at startup
#' - `active_session_counter`: initialized to 0
#'
#' It also logs startup and shutdown messages to the console.
#'
#' @return No return value. Used for side effects.
#' @examples
#' if (interactive()) {
#'   app_start_up_function()
#' }
#' @export
app_start_up_function <- function() {
  application_token       <<- shiny:::createUniqueId(8)
  application_start_time  <<- Sys.time()
  active_session_counter  <<- 0
  
  message(
    application_start_time,
    " Application ", application_token,
    " started."
  )
  
  message(
    Sys.time(),
    " Application ", application_token,
    ": ",
    active_session_counter, " active sessions."
  )
  
  onStop(function() {
    message(
      Sys.time(),
      " Application ", application_token,
      " stopped."
    )
  })
}


#' Insert or update a user in the users_table
#'
#' @param pool A DBI connection pool object
#' @param user_first_name First name of the user
#' @param user_last_name Last name of the user
#' @param user_session Session token (used to identify the user)
#' @param workshop_set Workshop selection
#' @param round_number Round number
#' @param question_number Question number
#'
#' @return None. Inserts or updates the row in the users_table.
upsert_user_info <- function(
    pool,
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_type
) {
  DBI::dbExecute(pool, "
    INSERT INTO users_table (
      user_first_name,
      user_last_name,
      user_session,
      workshop_set,
      round_number,
      question_number,
      question_type,
      created
    ) VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ON CONFLICT(user_session) DO UPDATE SET
      user_first_name = excluded.user_first_name,
      user_last_name = excluded.user_last_name,
      workshop_set = excluded.workshop_set,
      round_number = excluded.round_number,
      question_number = excluded.question_number,
      question_type = excluded.question_type,
      created = CURRENT_TIMESTAMP;
  ", params = list(
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_type
  ))
}


#' Insert a range response into the database
#'
#' @param pool A DBI connection pool object
#' @param user_first_name First name of the user
#' @param user_last_name Last name of the user
#' @param user_session Session token
#' @param workshop_set Workshop selection
#' @param round_number Round number
#' @param question_number Question number
#' @param question_text The question text
#' @param index_in_set Index of the question in the set
#' @param lower_90 Lower bound of 90% confidence interval
#' @param upper_90 Upper bound of 90% confidence interval
#' @param truth The correct answer (numeric)
#' @param relative_error Relative error (numeric)
#' @param source Source of the question
#'
#' @return None. Inserts a row into the range_responses table.
insert_range_response <- function(
    pool,
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_text,
    index_in_set,
    lower_90,
    upper_90,
    truth,
    relative_error
    ) {
  DBI::dbExecute(pool, "
    INSERT INTO range_responses (
      user_first_name,
      user_last_name,
      user_session,
      workshop_set,
      round_number,
      question_number,
      question_text,
      index_in_set,
      lower_90,
      upper_90,
      truth,
      relative_error
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
  ", params = list(
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_text,
    index_in_set,
    lower_90,
    upper_90,
    truth,
    relative_error
  ))
}


#' Insert a binary response into the database
#'
#' @param pool A DBI connection pool object
#' @param user_first_name First name of the user
#' @param user_last_name Last name of the user
#' @param user_session Session token
#' @param workshop_set Workshop selection
#' @param round_number Round number
#' @param question_number Question number
#' @param question_text The question text
#' @param index_in_set Index of the question in the set
#' @param response User's binary response (TRUE/FALSE)
#' @param confidence Confidence as a string (e.g., "70%")
#' @param truth The correct answer (TRUE/FALSE)
#' @param brier_score Brier score as a numeric value
#'
#' @return None. Inserts a row into the binary_responses table.
insert_binary_response <- function(
    pool,
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_text,
    index_in_set,
    response,
    confidence,
    truth,
    brier_score
) {
  DBI::dbExecute(pool, "
    INSERT INTO binary_responses (
      user_first_name,
      user_last_name,
      user_session,
      workshop_set,
      round_number,
      question_number,
      question_text,
      index_in_set,
      response,
      confidence,
      truth,
      brier_score
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
  ", params = list(
    user_first_name,
    user_last_name,
    user_session,
    workshop_set,
    round_number,
    question_number,
    question_text,
    index_in_set,
    response,
    confidence,
    truth,
    brier_score
  ))
}

load_users_range_responses <- function(pool, first_name, last_name, session, workshop) {
  query <- glue::glue_sql(
    "SELECT * FROM range_responses
     WHERE user_first_name = {first_name}
       AND user_last_name = {last_name}
       AND user_session = {session}
       AND workshop_set = {workshop}",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)
}

load_users_binary_responses <- function(pool, first_name, last_name, session, workshop) {
  query <- glue::glue_sql(
    "SELECT * FROM binary_responses
     WHERE user_first_name = {first_name}
       AND user_last_name = {last_name}
       AND user_session = {session}
       AND workshop_set = {workshop}",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)
}

# Function to codify, compress, encrypt, and URL encode the data
base_encrypt <- function(codified_string, key, nonce) {
  compressed_string <- memCompress(charToRaw(codified_string), type = "gzip")
  encrypted_string <- sodium::data_encrypt(compressed_string, key, nonce)
  encoded_string <- base64enc::base64encode(encrypted_string)
  url_encoded_string <- URLencode(encoded_string, reserved = TRUE)
  return(url_encoded_string)
}

# Function to decode, decompress, decrypt, and URL decode the data
base_decrypt <- function(url_encoded_string, key, nonce) {
  encoded_string <- URLdecode(url_encoded_string)
  decoded_encrypted_string <- base64enc::base64decode(encoded_string)
  decrypted_compressed <- sodium::data_decrypt(decoded_encrypted_string, key, nonce)
  decompressed_string <- rawToChar(memDecompress(decrypted_compressed, type = "gzip"))
  return(decompressed_string)
}

encrypt_question_index <- function(data, key, nonce) {
  codified_string <- paste(
    data$Type, data$Round, data$Number, sep = "x", collapse = "/"
  )
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}

decrypt_question_index <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "/")[[1]]
  decompressed_question_index <- map_dfr(rows, ~ {
    values <- strsplit(.x, "x")[[1]]
    tibble(Type = as.character(values[1]), Round = as.integer(values[2]), Number = as.integer(values[3]))
  })
  return(decompressed_question_index)
}

