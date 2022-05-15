


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




test_that("as_http_error preserves data in error object", {

  err <- errorCondition("a simple error", iris = iris)

  http_err <- as_http_error(err, cars = cars)

  expect_identical(http_err$status, 500L)
  expect_identical(http_err$iris, iris)
  expect_identical(http_err$cars, cars)



})

# as_http_error ------------------------------------------------------

test_that("as_http_error_body() works with base errorConditions", {
  x <- errorCondition("a message", some_data = 123)
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_false("call" %in% names(body))
})




test_that("as_http_error_body() works for regularily cought errors", {
  x <- tryCatch(stop("a simple error"), error = identity)
  body <- as_http_error_body(x)
  expect_s3_class(body, "list")
  expect_s3_class(body, "http_error_body")
  expect_true("call" %in% names(as_http_error_body(x)))
})




test_that("as_http_error_body() works with rlang errors", {
  x <- tryCatch(rlang::abort("an rlang error"), error = identity)
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
})




test_that("as_http_error_body() works with httr error responses", {
  x <- httr::GET("http://www.google.com/oidwavwevjsdnytwethweoi")
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_identical(body[["status"]], 404L)
})




test_that("as_http_error_body() works with httr2 errors", {
  x <- tryCatch(
    httr2::req_perform(httr2::request("http://127.0.0.1/sdklubngv489bvt")),
    error = identity
  )

  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_identical(body[["status"]], 404L)
})


# as_http_error_body ------------------------------------------------------

test_that("as_http_error_body() works with base errorConditions", {
  x <- errorCondition("a message", some_data = 123)
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_false("call" %in% names(body))
})




test_that("as_http_error_body() works for regularily cought errors", {
  x <- tryCatch(stop("a simple error"), error = identity)
  body <- as_http_error_body(x)
  expect_s3_class(body, "list")
  expect_s3_class(body, "http_error_body")
  expect_true("call" %in% names(as_http_error_body(x)))
})




test_that("as_http_error_body() works with rlang errors", {
  x <- tryCatch(rlang::abort("an rlang error"), error = identity)
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
})




test_that("as_http_error_body() works with httr error responses", {
  x <- httr::GET("http://www.google.com/oidwavwevjsdnytwethweoi")
  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_identical(body[["status"]], 404L)
})




test_that("as_http_error_body() works with httr2 errors", {
  x <- tryCatch(
    httr2::req_perform(httr2::request("http://127.0.0.1/sdklubngv489bvt")),
    error = identity
  )

  body <- as_http_error_body(x)
  expect_s3_class(body, "http_error_body")
  expect_identical(body[["status"]], 404L)
})
