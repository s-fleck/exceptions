# http --------------------------------------------------------------------

#' HTTP errors
#'
#' Special error classes for HTTP. See also [as_error_body] for turning R errors
#' into lists that can be returned by plumber APIs
#'
#' @export
#' @seealso https://tools.ietf.org/html/rfc7231#section-6
HttpError <- function(
  message,
  status,
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  assert(is_scalar_integerish(status))
  assert(identical(nchar(as.character(status)), 3L))

  status   <- as.integer(status)
  # compat with httr error conditions
  status_class  <- paste0("http_", status)
  status_parent <- paste0("http_", (status %/% 100) * 100)

  errorCondition(
    message = message,
    status = status,
    header = headers,
    ...,
    class = union(class, c(status_class, status_parent, "http_error", "HttpError")),
    call = call
  )
}




# Server Errors -----------------------------------------------------------

#' @rdname HttpError
#' @export
HttpServerError <- function(
  message,
  status,
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  assert(status %in% 500:599, "Server error status codes must be between 500 und 599")

  HttpError(
    message = message,
    status = status,
    ...,
    headers = headers,
    class = union(class, "HttpServerError"),
    call = call
  )
}




#' @rdname HttpError
#' @export
HttpInternalServerError <- function(
  message = "Internal Server Error",
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  HttpServerError(
    message = message,
    status = 500L,
    ...,
    headers = headers,
    class = union(class, "HttpInternalServerError"),
    call = call
  )
}




#' @rdname HttpError
#' @export
HttpServiceUnavailableError <- function(
  message = "Service Unavailable",
  ...,
  headers = NULL,
  class = NULL,
  call = NULL,
  retry_after = NULL
){
  if (!is.null(retry_after)){
    if (is.null(headers)){
      headers <- list()
    } else {
      assert(is.list(headers))
    }
    headers <- c(headers, list("retry-after" = retry_after))
  }

  HttpServerError(
    message = message,
    status = 503L,
    ...,
    headers = headers,
    class = union(class, "HttpServiceUnavailableError"),
    call = call
  )
}





# Client Errors -----------------------------------------------------------

#' @rdname HttpError
#' @export
HttpClientError <- function(
  message,
  status,
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  assert(status %in% 400:499, "Client error status codes must be between 500 und 599")

  HttpError(
    message = message,
    status = status,
    ...,
    headers = headers,
    class = union(class, "HttpClientError"),
    call = call
  )
}







#' @rdname HttpError
#' @export
HttpBadRequestError <- function(
  message = "Bad Request",
  ...,
  headers = headers,
  class = NULL,
  call = NULL
){
  HttpClientError(
    message = message,
    status = 400L,
    ...,
    headers = headers,
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
