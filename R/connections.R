#' Connection Errors
#'
#' @export
ConnectionError <- function(
  message = "Connection error",
  ...,
  class = NULL,
  call = NULL
){
  OsError(
    message = message,
    ...,
    class = union(class, "ConnectionError"),
    call = call
  )
}




#' Raised when a connection is reset by the peer. Corresponds to errno ECONNRESET.
#' @rdname HttpError
#' @export
ConnectionResetError <- function(
  message = "Connection reset by peer",
  ...,
  class = NULL,
  call = NULL
){
  ConnectionError(
    message = message,
    ...,
    class = union(class, "ConnectionResetError"),
    call = call
  )
}




#' Raised when a connection attempt is refused by the peer. Corresponds to errno ECONNREFUSED.
#' @rdname ConnectionError
#' @export
ConnectionRefusedError <- function(
  message = "Connection refused by peer",
  ...,
  class = NULL,
  call = NULL
){
  ConnectionError(
    message = message,
    ...,
    class = union(class, "ConnectionRefusedError"),
    call = call
  )
}




#' Rraised when a connection attempt is aborted by the peer. Corresponds to errno ECONNABORTED.
#'
#' @rdname ConnectionError
#' @export
ConnectionAbortedError <- function(
  message = "Connection aborted by peer",
  ...,
  class = NULL,
  call = NULL
){
  ConnectionError(
    message = message,
    ...,
    class = union(class, "ConnectionAbortedError"),
    call = call
  )
}

