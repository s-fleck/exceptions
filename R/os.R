#' Connection Errors
#'
#' @export
OsError <- function(
  message = "Os-level Error",
  ...,
  class = NULL,
  call = NULL
){
  errorCondition(
    message = message,
    ...,
    class = union(class, "OsError"),
    call = call
  )
}




#' Raised when a connection is reset by the peer. Corresponds to errno ECONNRESET.
#' @rdname HttpError
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


