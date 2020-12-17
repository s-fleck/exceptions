#' @rdname errors
#' @export
FileExistsError <- function(
  message = if (is.character(file)) sprintf("file exists: '%s'", file) else "file does not exist",
  ...,
  file = NULL,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "FileExistsError"), call = call)
}




#' @rdname errors
#' @export
FileNotFoundError <- function(
  message = if (is.character(file)) sprintf("file does not exist: %s", file) else "file does not exist",
    ...,
  file = NULL,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "FileNotFoundError"), call = call)
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
DirNotEmptyError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirNotEmptyError"), call = call)
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
DirNotFoundError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirNotFoundError"), call = call)
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
IsNotDirError <- function(
  message = sprintf("'%s' is not a directory.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "IsNotDirError"), call = call)
}


