condition <- function(message, ..., class = NULL, call = NULL){
  structure(
    list(
      message = as.character(message),
      call = call,
      ...),
    class = union(class, c("condition"))
  )
}




error <- function(message, ..., class = NULL, call = NULL){
  structure(
    list(
      message = as.character(message),
      call = call,
      ...),
    class = union(class, c("error", "condition"))
  )
}
