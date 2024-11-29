app_start_up_function <- function() {
  
  application_token <<- shiny:::createUniqueId(8)
  application_start_time <<- Sys.time()
  active_session_counter <<- 0
  
  message(
    application_start_time,
    " Application ", application_token,
    " started."
  )
  
  message(
    Sys.time(),
    " Application ",
    application_token, 
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

