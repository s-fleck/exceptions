ValueError <- function(
  message,
  ...,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "ValueError"))
}




NotImplementedError <- function(
  message = sprintf("functionality is not yet implemented", dir),
  ...,
  dir,
  class = NULL,
  call = NULL
){
  error(message, ..., class = c(class, "NotImplementedError"), call = call)
}
