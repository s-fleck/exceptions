# http --------------------------------------------------------------------

#' HTTP errors
#'
#' Special error classes for HTTP. See also [as_error_body] for turning R errors
#' into lists that can be returned by plumber apis.
#'
#'
#'
#' @export
HttpError <- function(
  message,
  http_status,
  ...,
  class = NULL,
  call = NULL
){
  assert(is_scalar_integerish(http_status))
  assert(identical(nchar(as.character(http_status)), 3L))

  http_status   <- as.integer(http_status)
  status_class  <- paste0("http_", http_status)
  status_parent <- paste0("http_", (http_status %/% 100) * 100)

  errorCondition(
    class = union(class, c("HttpError", status_class, status_parent, "http_error")),
    message = message,
    http_status = http_status,
    ...
  )
}


#' @rdname HttpError
#' @export
HttpServiceUnavailableError <- function(
  message = "Service unavailable",
  ...
){
  HttpError(
    class = "HttpServiceUnavailableError",
    message = message,
    http_status = 503,
    ...
  )
}




#' @rdname HttpError
#' @export
HttpBadRequestError <- function(
  message = "HTTP 400 (Bad Request)",
  http_status = 400,
  ...,
  class = NULL,
  call = NULL
){
  HttpError(
    message = message,
    http_status = http_status,
    ...,
    class = union(class, "HttpBadRequestError"),
    call = call
  )
}








# json api error objects ---------------------------------------------------

#' Error Response Body
#'
#' @param type `character` scalar. Type of the error (usually `class(e)[[1]]`)
#' @param message `character` scalar. an error message
#' @param error `character` an [base::errorCondition] object
#' @param ... arbitrary \R obejcts, as long as plumber knows how to serialize
#'   them
#'
#' @return A `list` of subclass `error_body` with elements `error`
#'   (the http), `type`,
#'   and `message`
#'
#' @export
#' @seealso [errors]
error_body <- function(
  message,
  ...,
  class = class
){
  assert(is.character(class))
  assert(is_scalar_character(message))

  structure(
    list(
      message = as.character(message),
      class = class,
      ...
    ),
    class = c("error_body", "list")
  )
}



#' `as_error_body()` converts \R condition objects to lists that plumber knows
#' how to serialze to.
#'
#' @param e a `condition` S3 object (usually an `error`)
#'
#'
#' @rdname error_body
#' @export
as_error_body <- function(
  e
){
  UseMethod("as_error_body")
}




#' @rdname error_body
#' @export
as_error_body.default <- function(
  e
){
  assert(inherits(e, "error") || inherits(e, "list"))

  if (inherits(e, "error")){
    res <- tryCatch(
      c(unclass(e), list(class = class(e))),
      error = function(e) message = paste("Cannot serialize error condition object: ", e)
    )
  }

  res <- compact(res)

  if ("call" %in% names(res)){
    res[["call"]] <- format(res[["call"]])
  }

  if (!"message" %in% names(res)){
    message <- "Internal Server Error"
  } else {
    res[["message"]] <- as.character(res$message)
  }


  do.call(error_body, res)
}
