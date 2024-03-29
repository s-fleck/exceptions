#' HTTP errors
#'
#' Special error classes for HTTP. See also [as_http_error_body()] for
#' turning R errors into lists that can be returned by plumber APIs.
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
  status <- normalize_http_status(status)

  # compat with httr error conditions
  status_class  <- paste0("http_", status)
  status_parent <- paste0("http_", (status %/% 100L) * 100L)

  error(
    message = message,
    status = status,
    headers = headers,
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




# coercion -------------------------------------------------------------



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
as_http_error.error <- function(
    x,
    ...
){
  x <- as.list(x)
  dots <- list(...)
  x <- x[!names(x) %in% names(dots)]

  do.call(HttpError, c(as.list(x), list(...)))
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
    class = "HttrResponseError"
){
  assert_namespace("httr")
  status  <- httr::status_code(x)
  message <- httr::http_status(x)$reason

  HttpError(
    message = message,
    status = status,
    response = x,
    ...,
    class = class,
    call = call
  )
}




#' @inheritParams HttpError
#' @rdname as_http_error
#' @export
as_http_error.httr2_response <- function(
    x,
    ...,
    class = NULL,
    call = NULL
){
  assert_namespace("httr2")
  status  <- httr2::resp_status(x)
  message <- httr2::resp_status_desc(x)
  headers <- httr2::resp_headers(x)

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




#' @inheritParams HttpError
#' @rdname as_http_error
#' @export
as_http_error.httr2_http <- function(
    x,
    ...,
    class = NULL,
    call = NULL
){
  if ("resp" %in% names(x)){
    as_http_error(x[["resp"]])
  } else {
    as_http_error.error(resp)
  }
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
  class = NULL
){
  message <- paste(as.character(message), collapse = " ")

  assert(is.null(class) || is.character(class))
  assert(is_scalar_character(message))

  structure(
    compact(list(
      message = message,
      class = class,
      ...
    )),
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
as_http_error_body.list <- function(
  e
){
  e <- compact(e)
  e <- lapply(e, function(.) tryCatch(format(.), error = function(.) NULL))

  if (!"message" %in% names(e)){
    e[["message"]] <- "Internal Server Error"
  }

  e[["status"]] <- as.integer(e[["status"]])

  if (!is_scalar_integerish(e[["status"]])){
    e[["status"]] <- 500L
  }

  do.call(http_error_body, e)
}




#' @rdname http_error_body
#' @export
as_http_error_body.error <- function(
  e
){
  as_http_error_body(unclass(e))
}



#' @rdname http_error_body
#' @export
as_http_error_body.rlang_error <- function(
  e
){
  e <- unclass(e)

  if ("trace" %in% names(e)){
    tryCatch({
      # in two steps, so that the trace still gets formatted in the unlikely
      # case that crayon is not available
      e[["trace"]] <- format(e[["trace"]])
      e[["trace"]] <- crayon::strip_style(format(e[["trace"]]))
    }, error = function(e) NULL)
  }
  as_http_error_body(e)
}




#' @rdname http_error_body
#' @export
as_http_error_body.httr2_http <- function(
  e
){
  content_type <- tolower(httr2::resp_content_type(e$resp))

  tryCatch({
    if (identical(content_type, "application/json")){
      e[["response_body"]] <- httr2::resp_body_json(e$resp)
    } else if (content_type %in% c("application/xml", "text/xml")){
      e[["response_body"]] <- httr2::resp_body_xml(e$resp)
    } else {
      e[["response_body"]] <- httr2::resp_body_string(e$resp)
    }
  }, error = function(e) NULL)

  e[["resp"]] <- NULL

  as_http_error_body.rlang_error(e)
}




#' @rdname http_error_body
#' @export
as_http_error_body.response <- function(
  e
){
  as_http_error_body(as_http_error(e))
}




# utils -------------------------------------------------------------------

normalize_http_status <- function(x){
  internal_server_error_status <- 500L

  if (missing(x)){
    return(internal_server_error_status)
  }

  if (!is_scalar_atomic(x)){
    return(internal_server_error_status)
  }

  x <- as.integer(x)

  if (is.na(x)){
    return(internal_server_error_status)
  }

  x
}
