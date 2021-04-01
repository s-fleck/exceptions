#' HTTP errors
#'
#' Special error classes for HTTP. See also [as_http_error_body] for turning R errors
#' into lists that can be returned by plumber APIs.
#'
#' @return an `error condition` object with the subclasses
#' `HttpError`, `http_error` and `http_<status>`. The latter two are for
#' compatability with [httr::http_condition()] objects. The error conditions
#' reflect the hirarchy, e.g. `HttpServiceUnavailableError` inherits from
#' `HttpServerError`, `HttpError`, `http_500`, `http_error`.
#'
#' @export
#' @seealso https://tools.ietf.org/html/rfc7231#section-6
#'
#' @examples
#' print(class(HttpServiceUnavailableError()))
#'
#' tryCatch(
#'   stop(HttpServiceUnavailableError()),
#'   HttpServiceUnavailableError = function(e) print(e)
#' )
#'
#' tryCatch(
#'   stop(HttpServiceUnavailableError()),
#'   http_503 = function(e) print(e)
#' )
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
  status_parent <- paste0("http_", (status %/% 100L) * 100L)

  errorCondition(
    message = message,
    status = status,
    headers = headers,
    ...,
    class = union(class, c(status_class, status_parent, "http_error", "HttpError")),
    call = call
  )
}



#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_http_error <- function(
  x,
  ...
){
  UseMethod("as_http_error")
}

#' Title
#'
#' @param x
#' @param ...
#' @param class
#' @param call
#'
#' @return
#' @export
#'
#' @examples
as_http_error.response <- function(
  x,
  ...,
  class = NULL,
  call = NULL
){
  status  <- httr::status_code(x)
  message <- httr::http_status(x)$reason
  headers <- httr::headers(x)

  HttpError(
    message = message,
    status = status,
    headers = headers,
    response = x,
    ...,
    class = class,
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
  headers = NULL,
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




#' @rdname HttpError
#' @export
HttpNotFoundError <- function(
  message = "Not Found",
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  HttpClientError(
    message = message,
    status = 404L,
    ...,
    headers = headers,
    class = union(class, "HttpNotFoundError"),
    call = call
  )
}




#' @rdname HttpError
#' @export
HttpConflictError <- function(
  message = "Conflict",
  ...,
  headers = NULL,
  class = NULL,
  call = NULL
){
  HttpClientError(
    message = message,
    status = 409L,
    ...,
    headers = headers,
    class = union(class, "HttpConflictError"),
    call = call
  )
}




# json api error objects ---------------------------------------------------

#' Handle HttpErrors in plumber APIs
#'
#' @param req a plumber Request object
#' @param res a plumber Response object
#' @param err the error
#'
#' @return an [http_error_body] list object
#' @export
#' @seealso
#' * https://github.com/rstudio/plumber/pull/507#issuecomment-675693897
#' * https://www.rplumber.io/
#'
#' @examples
#' \dontrun{
#'
#' #* @plumber
#' function(pr) {
#'   pr_set_error(pr, handle_http_error)
#' }
#'
#'
handle_http_error <- function(
  req,
  res,
  err
){
  s <- err$status
  res$status <- if (is.null(s)) 500L else s

  for (h in names(err$headers)){
    res$setHeader(h, err$headers[[h]])
  }

  as_http_error_body(err)
}




#' Error Response Body
#'
#' @param type `character` scalar. Type of the error (usually `class(e)[[1]]`)
#' @param message `character` scalar. an error message
#' @param error `character` an [base::errorCondition] object
#' @param ... arbitrary \R obejcts, as long as plumber knows how to serialize
#'   them
#'
#' @return A `list` of subclass `http_error_body` with elements `error`
#'   (the http), `type`,
#'   and `message`
#'
#' @export
#' @seealso [errors]
http_error_body <- function(
  message,
  ...,
  class = class
){
  message <- paste(as.character(message), collapse = " ")

  assert(is.character(class))
  assert(is_scalar_character(message))

  structure(
    list(
      message = message,
      class = class,
      ...
    ),
    class = c("http_error_body", "list")
  )
}



#' `as_http_error_body()` converts \R condition objects to lists that plumber knows
#' how to serialze to.
#'
#' @param e a `condition` S3 object (usually an `error`)
#'
#'
#' @rdname http_error_body
#' @export
as_http_error_body <- function(
  e
){
  UseMethod("as_http_error_body")
}




#' @rdname http_error_body
#' @export
as_http_error_body.default <- function(
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

  res[["content"]] <- tryCatch(
    format(httr::content(res)),
    error = function(e) NULL
  )

  res <- compact(res)

  do.call(http_error_body, res)
}
