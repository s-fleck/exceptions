test_that("http works as expected", {

  x <- try(stop(), silent = TRUE)
  r <- as_http_error_body(attr(x, "condition"))

  expect_s3_class(r, "list")
})




test_that("as_http_error works as expected", {

  x <- httr::GET("www.google.com/oidwavwevjsdnytwethweoi")

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
