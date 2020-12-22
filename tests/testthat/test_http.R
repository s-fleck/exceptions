context("http")


test_that("http works as expected", {

  x <- try(stop(), silent = TRUE)
  r <- as_http_error_body(attr(x, "condition"))

  expect_s3_class(r, "list")


})
