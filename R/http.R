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
    class = union(class, c(status_class, status_parent, "HttpError", "http_error")),
    message = message,
    http_status = http_status,
    ...
  )
}


#' @rdname HttpError
#' @export
HttpServiceUnvailableError <- function(
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
#' @param error `character` scalar. The http error as character scalar
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
  type,
  message,
  error = "500 - Internal Server Error",
  ...
){
  assert(is_scalar_character(type))
  assert(is_scalar_character(message))
  assert_namespace("jsonlite")
  structure(
    list(
      error = error,
      type = jsonlite::unbox(type),
      message = jsonlite::unbox(message),
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

  if (!"http_status" %in% names(e))
    e$http_status <- 500

  if (!"message" %in% names(e))
    e$message <- "Internal Server Error"

  res <- error_body(
    error = paste(e$http_status, e$message),
    type = class(e)[[1]],
    message = e$message
  )

  for (nm in setdiff(names(e), c("message", "call"))){
    res[[nm]] <- e[[nm]]
  }

  res
}
