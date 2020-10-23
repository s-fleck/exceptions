#' @rdname errors
#' @export
FileExistsError <- function(
  message = if (is.character(file)) sprintf("file exists: '%s'", file) else "file does not exist",
  file = NULL
){
  errorCondition(message = message, class = "FileExistsError")
}




#' @rdname errors
#' @export
FileNotFoundError <- function(
  message = if (is.character(file)) sprintf("file does not exist: %s", file) else "file does not exist",
  file = NULL
){
  errorCondition(
    message = message,
    class = c("FileNotFoundError")
  )
}




DirIsNotEmptyError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirIsNotEmptyError"), call = call)
}




DirDoesNotExistError <- function(
  message = sprintf("directory '%s' does not exist.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "DirDoesNotExistError"), call = call)
}




IsNotADirError <- function(
  message = sprintf("'%s' is not a directory.", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "IsNotADirError"), call = call)
}


