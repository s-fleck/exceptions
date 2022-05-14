#' System-related error
#'
#' Raised when a system function returns a system-related error, including I/O
#' failures such as “file not found” or “disk full” (not for illegal argument
#' types or other incidental errors).
#'
#' @export
OsError <- function(
  message = "Os-level Error",
  ...,
  class = NULL,
  call = NULL
){
  error(
    message = message,
    ...,
    class = union(class, "OsError"),
    call = call
  )
}




#' Timeout error
#'
#' Raised when a system function timed out at the system level. Corresponds to errno ETIMEDOUT.
#' @rdname OsError
#' @export
TimeoutError <- function(
  message = "Timeout Error",
  ...,
  class = NULL,
  call = NULL
){
  OsError(
    message = message,
    ...,
    class = union(class, "TimeoutError"),
    call = call
  )
}


