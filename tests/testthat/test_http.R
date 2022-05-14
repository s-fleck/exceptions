test_that("http works as expected", {

  x <- try(stop(), silent = TRUE)
  r <- as_http_error_body(attr(x, "condition"))

  expect_s3_class(r, "list")
})




test_that("as_http_error works as expected", {

  x <- httr::GET("http://www.google.com/oidwavwevjsdnytwethweoi")

  stop_if_http_error <- function(x){
    if (httr::http_error(x)){
      stop(as_http_error(x))
    } else {
      TRUE
    }
  }

  expect_error(stop_if_http_error(x), class = "http_error")
  expect_error(stop_if_http_error(x), class = "HttpError")
  expect_error(stop_if_http_error(x), class = "HttpError")
  expect_error(stop_if_http_error(x), class = "http_404")
})




test_that("as_error_body works with simple errorConditions", {
  x <- errorCondition("a message", some_data = 123)
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_false("call" %in% names(body))
})




test_that("as_error_body works with simple errors", {
  x <- tryCatch(
    stop("a simple error"),
    error = function(e) {
      as_http_error_body(e)
    }
  )

  body <- as_http_error_body(x)
  expect_s3_class(as_http_error_body(x), "http_error_body")
  expect_true("call" %in% names(as_http_error_body(x)))
})




test_that("as_error_body works with rlang errors", {
  x <- tryCatch(
    rlang::abort("an rlang error"),
    error = function(e) as_http_error_body(e)
  )

  expect_s3_class(as_http_error_body(x), "http_error_body")
})



test_that("as_http_error works with httr2 errors", {
  x <- tryCatch(
    httr2::req_perform(httr2::request("http://127.0.0.1/sdklubngv489bvt")),
    error = function(e) as_http_error_body(e)
  )

  expect_s3_class(as_http_error_body(x), "http_error_body")
})




test_that("as_http_error works with httr errors", {
  x <- httr::GET("http://www.google.com/oidwavwevjsdnytwethweoi")
  expect_s3_class(as_http_error_body(x), "http_error_body")
})
