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
