#' Title
#'
#' @param message
#' @param ...
#' @param class
#' @param call
#'
#' @return
#' @export
#'
#' @examples
ValueError <- function(
  message,
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "ValueError"), call = call)
}




#' Title
#'
#' @param message
#' @param ...
#' @param dir
#' @param class
#' @param call
#'
#' @return
#' @export
#'
#' @examples
NotImplementedError <- function(
  message = sprintf("functionality is not yet implemented"),
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "NotImplementedError"), call = call)
}
