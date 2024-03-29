# sfmisc utils 1.1.1


# utils -------------------------------------------------------------------

# nocov start
# commonly used utility functions included from the package sfmisc


#' Paste and Truncate
#'
#' @param x a vector
#' @param width (maximum) width of result
#' @param dots `character` scalar. String to use for ellipses
#' @inheritParams base::paste
#'
#' @return a `character` scalar
#' @noRd
#'
#' @examples
#'   ptrunc(month.abb)
#'   ptrunc(month.abb, month.name)
ptrunc <- function(
  ...,
  width = 40L,
  sep = ", ",
  collapse = ", ",
  dots = " ..."
){
  assert(width > 7L, "The minimum supported width is 8")
  x <- paste(..., sep = sep, collapse = collapse)

  sel <- vapply(x, nchar, integer(1), USE.NAMES = FALSE) > width

  x[sel] <- strtrim(x[sel], width = width - 4L)
  x[sel] <- paste0(gsub(",{0,1}\\s*$", "", x[sel]), dots)
  x
}



#' Format class of an object
#'
#' @param open,close opening and closing bracket to use for formatting
#'
#' @noRd
#' @examples
#' x <- iris
#' class(x) <- c("flowers", "data.frame")
#' class_fmt(x)
#' fmt_class(class(x))
fmt_class <- function(x, open = "<", close = ">"){
  paste0(open, paste(x, collapse = "/"), close)
}




#' @param x any \R object
#' @param ignore subclasses to ignore
#'
#' @rdname fmt_class
#' @noRd
class_fmt <- function(x, ignore = NULL, open = "<", close = ">"){
  fmt_class(setdiff(class(x), ignore), open = open, close = close)
}




#' Remove NULLs from an object (usually a list)
#' @noRd
compact <- function(x){
  x[!vapply(x, is.null, FALSE)]
}




#' Apply a function to each element of a list or vector (without returning anything)
#'
#' Like lapply, but return `.x` instead of the results of `.f`
#'
#' @param .x an atomic `vector` or a `list`
#' @param .f a `function`
#' @param ... arguments passed on to `.f`
#'
#' @return `.x`
#'
#' @noRd
#' @examples
#' walk(month.name, print)
walk <- function(.x, .f, ...){
  for (i in seq_along(.x)){
    .f(.x[[i]], ...)
  }

  invisible(.x)
}




# assertions --------------------------------------------------------------

#' Assert a condition
#'
#' A simpler and more efficient replacement for [base::stopifnot()].
#' As opposed to `stopifnot()`, `assert()` only works with a single (scalar) assertions.
#'
#' @param cond `TRUE` or `FALSE` (without any attributes).
#'   * `TRUE` will return `TRUE`
#'   * `FALSE` will throw an exception with an automatically constructed
#'     error if none was supplied in `...`.
#'   * any other value will throw an error indicating that `cond` was illegal
#' @param ... Either `character` scalars that will be combined with [base::paste0()]
#'   or a single `condition` object.
#'
#' @noRd
#'
#' @return `TRUE` on success
#' @seealso [assert_all()]
#' @examples
#' \dontrun{
#'   assert(1 == 1)
#'   assert(1 == 2)
#'   assert(1 == 2, "one is not ", "two")
#'   assert(1 == 2, errorCondition("one is not two", class = "ObviousError"))
#' }
assert <- function(
  cond,
  ...,
  call = sys.call(-1)
){
  if (identical(cond, TRUE)){
    return(TRUE)
  }

  if (identical(cond, FALSE)){

    dots <- list(...)

    if (identical(length(dots), 0L)){
      msg <- paste0("`", deparse(match.call()[[2]]), "`", " is not 'TRUE'")
    } else {

      if (inherits(dots[[1]], "condition")){
        stop(dots[[1]])
      }

      msg <- suppressWarnings(paste0(...))
    }

  } else {
    msg <- "Assertion must be either 'TRUE' or 'FALSE'"
  }

  error <- structure(
    list(
      message = as.character(msg),
      call = call
    ),
    class = c("assertError", "error", "condition")
  )

  stop(error)
}




#' Assert a package is installed
#' @noRd
assert_namespace <- function(...){
  pkgs <- c(...)

  res <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (all(res)){
    return(invisible(TRUE))

  } else {
    miss <- pkgs[!res]

    if (identical(length(miss), 1L)){
      msg <- sprintf(paste(
        "This function requires the package '%s'. You can install it with",
        '`install.packages("%s")`.'), miss, miss
      )
    } else {
      msg <- sprintf(
        paste(
          "This function requires the packages %s. You can install them with",
          "`install.packages(%s)`."
        ),
        paste(miss, collapse = ", "),
        paste0("c(", paste(paste0('\"', miss, '\"'), collapse = ", "), ")")
      )
    }
  }

  stop(msg, call. = FALSE)
}




# predicates --------------------------------------------------------------

#' Convenient predicate functions
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
is_error <- function(x){
  inherits(x, "error")
}




#' @rdname is_error
#' @noRd
is_try_error <- function(x){
  inherits(x, "try-error")
}




#' @rdname is_error
#' @noRd
is_scalar <- function(x){
  identical(length(x), 1L)
}




#' @rdname is_error
#' @noRd
is_POSIXct <- function(x){
  inherits(x, "POSIXct")
}




#' @rdname is_error
#' @noRd
is_scalar_POSIXct <- function(x){
  is_POSIXct(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_POSIXlt <- function(x){
  inherits(x, "POSIXlt")
}




#' @rdname is_error
#' @noRd
is_scalar_POSIXlt <- function(x){
  is_POSIXlt(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_POSIXt <- function(x){
  inherits(x, "POSIXt")
}




#' @rdname is_error
#' @noRd
is_scalar_POSIXt <- function(x){
  is_POSIXt(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_Date <- function(x){
  inherits(x, "Date")
}




#' @rdname is_error
#' @noRd
is_scalar_Date <- function(x){
  is_Date(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_list <- function(x){
  is_list(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_atomic <- function(x){
  is.atomic(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_integer <- function(x){
  is.integer(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_factor <- function(x){
  is.factor(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_list <- function(x){
  is.list(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_numeric <- function(x){
  is.numeric(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




#' @rdname is_error
#' @noRd
is_vector <- function(x){
  is.atomic(x) || is.list(x)
}




#' @rdname is_error
#' @noRd
is_bool <- function(x){
  is.logical(x) && !anyNA(x)
}




#' Check if Object is a Boolean
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
is_scalar_bool <- function(x){
  identical(x, TRUE) || identical(x, FALSE)
}




#' Check if Object is Integer-like
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
is_integerish <- function(x, na_rm = FALSE){

  if (na_rm){
    x <- x[!is.na(x)]
  }

  if (!is.numeric(x)){
    FALSE
  } else {
    all(as.integer(x) == x)
  }
}




#' @rdname is_error
#' @noRd
is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




#' @rdname is_error
#' @noRd
is_n <- function(x){
  is_scalar_integerish(x) && identical(x > 0, TRUE)
}




#' @rdname is_error
#' @noRd
is_n0 <- function(x){
  is_scalar_integerish(x) && identical(x >= 0, TRUE)
}




#' Check if Objects have the same length
#'
#' @param ... Any number of \R Objects.
#'
#' @return either `TRUE` or `FALSE`
#' @noRd
is_equal_length <- function(...){
  lengths <- vapply(list(...), length, 1L)
  identical(length(unique(lengths)), 1L)
}




#' Check if Object has length 0
#'
#' Check wheter an object is either `TRUE` or `FALSE`.
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_empty <- function(x){
  identical(length(x), 0L)
}




#' Check if a String is Blank
#'
#' Check wheter a character vector contains only of spaces
#'
#' @param x Any \R Object.
#' @return either `TRUE` or `FALSE`
#' @noRd
#'
is_blank <- function(x){
  trimws(x) == ""
}




#' Test if a Vector or Combination of Vectors is a Candidate Key
#'
#' Checks if all elements of the atomic vector `x`, or the combination of
#' all elements of `x` if `x` is a `list`, are unique and neither `NA` or
#' `infinite`.
#'
#' @param x a atomic vector or a list of atomic vectors
#'
#' @return `TRUE/FALSE`
#' @noRd
#'
#' @examples
#'
#' is_candidate_key(c(1, 2, 3))
#' is_candidate_key(c(1, 2, NA))
#' is_candidate_key(c(1, 2, Inf))
#'
#' td <- data.frame(
#'   x = 1:10,
#'   y = 1:2,
#'   z = 1:5
#' )
#'
#' is_candidate_key(list(td$x, td$z))
#' # a data.frame is just a special list
#' is_candidate_key(td[, c("y", "z")])
is_candidate_key <- function(x){

  if (is.atomic(x)){
    # !is.infinite instead of is.finite because x can be a character vector
    length(x) >= 1 &&
    all(!is.infinite(x)) &&
    !any(is.na(x)) &&
    identical(length(unique(x)), length(x))
  } else if (is.list(x)){
    length(x) > 0 &&
    length(x[[1]] > 0) &&
    do.call(is_equal_length, x) &&
    all(vapply(x, function(.x) all(!is.infinite(.x)), logical(1))) &&
    all(vapply(x, function(.x) !any(is.na(.x)), logical(1))) &&
    !any(duplicated(as.data.frame(x)))
  }
}




#' @rdname is_error
#' @seealso  https://modern-sql.com/feature/is-distinct-from
#' @noRd
is_not_distinct_from <- function(x, y){
  ((x == y) & !is.na(x) & !is.na(y)) | (is.na(x) & is.na(y))
}




#' @rdname is_error
#' @noRd
is_distinct_from <- function(x, y){
  ((x != y) & !is.na(x) & !is.na(y)) | (is.na(x) != is.na(y))
}



#' @rdname is_error
#' @noRd
is_windows_path <- function(x){
  nchar(x) >= 2 & grepl("^[A-Za-z].*", x) & substr(x, 2, 2) == ":"
}




#' @rdname is_error
#' @noRd
is_dir <- function(x){
  dir.exists(x) & file.info(x)[["isdir"]]
}




#' @rdname is_error
#' @noRd
is_empty_dir <- function(x){
  is_dir(x) &&
  identical(length(list.files(x, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)), 0L)
}

# equalish ----------------------------------------------------------------

#' Check for equality within a tolerance level
#'
#' @param x,y `numeric` vectors
#' @param tolerance `numeric` scalar. tolerance level (absolute value). Defaults
#'   to `.Machine$double.eps^0.5` which is a sensible default for comparing
#'   floating point numbers.
#'
#' @return `equalish()` returns TRUE if the absolute difference between `x` and
#'   `y` is less than `tolerance`.
#' @noRd
#' @seealso [.Machine]
#'
#' @examples
#' a <- 0.7
#' b <- 0.2
#' a - b == 0.5
#' equalish(a - b, 0.5)
equalish <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert(is_scalar_numeric(tolerance) && tolerance >= 0)
  abs(x - y) < tolerance
}




#' @return `equalish_frac()` returns `TRUE` if the relative difference between
#'   `x` and `y` is smaller than `tolerance`. The relative difference is
#'   defined as `abs(x - y) / pmax(abs(x), abs(y))`. If both `x` and `y` are
#'   `0` the relative difference is not defined, but this function will still
#'   return `TRUE`.
#' @rdname equalish
#' @noRd
#' @examples
#' equalish_frac(1000, 1010, tolerance = 0.01)
#' equalish_frac(1000, 1010, tolerance = 0.009)
#' equalish_frac(0, 0)
equalish_frac <- function(x, y, tolerance = .Machine$double.eps ^ 0.5){
  assert(is_scalar_numeric(tolerance) && tolerance >= 0)
  res <- abs(x - y) / pmax(abs(x), abs(y)) < tolerance
  res[x == 0 & y == 0] <- TRUE
  res
}




# all_are -----------------------------------------------------------------

#' Convert vector if identical elements to scalar
#'
#' Returns `unique(x)` if all elements of `x` are identical, throws an error if
#' not.
#'
#' @inheritParams all_are_identical
#'
#' @return A scalar of the same type as `x`
#' @noRd
as_scalar <- function(x){
  res <- unique(x)
  if (is_scalar(res)){
    return(res)
  } else {
    stop("Not all elements of x are identical")
  }
}




#' Test if all elements of a vector are identical
#'
#' @param x any object that can be handled by [unique()] (usually a vector or
#'   list)
#' @param empty_value Value to return if function is called on a vector of
#'   length 0 (e.g. `NULL`, `numeric()`, ...)
#'
#' @noRd
#' @return `TRUE` or `FALSE`
#'
#' @examples
#' all_are_identical(c(1,2,3))
#' all_are_identical(c(1,1,1))
all_are_identical <- function(x, empty_value = FALSE) {
  assert(length(empty_value) <= 1)

  if (length(x) > 0L) {
    return(identical(length(unique(x)), 1L))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




#' Test if all elements of a vector are unique
#'
#' @inheritParams all_are_identical
#'
#' @return `TRUE` or `FALSE`
#'
#' @noRd
#'
#' @examples
#' all_are_distinct(c(1,2,3))
#' all_are_distinct(c(1,1,1))
all_are_distinct <- function(
  x,
  empty_value = FALSE
){
  assert(length(empty_value) <= 1)

  if (identical(length(x), 1L)) {
    return(TRUE)

  } else if (length(x) > 1L) {
    return(identical(length(unique(x)), length(x)))

  } else {

    if (is.null(x)){
      warning("'x' is NULL")
    } else {
      warning("'x' is an empty vector")
    }

    return(empty_value)
  }
}




n_distinct <- function(x){
  length(unique(x))
}




# misc --------------------------------------------------------------------


pad_left <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " ",
  preserve_na = FALSE
){
  diff <- pmax(width - nchar(paste(x)), 0L)
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  res <- paste0(padding, x)

  if (preserve_na)
    res[is.na(x)] <- NA_character_

  res
}




pad_right <- function(
  x,
  width = max(nchar(paste(x))),
  pad = " ",
  preserve_na = FALSE
){
  diff <- pmax(width - nchar(paste(x)), 0L)
  padding <-
    vapply(diff, function(i) paste(rep.int(pad, i), collapse = ""), character(1))
  paste0(x, padding)

  if (preserve_na)
    res[is.na(x)] <- NA_character_

  res
}




`%||%` <- function(x, y){
  if (is.null(x))
    y
  else (x)
}




preview_object <- function(
  x,
  width = 32,
  brackets = c("(", ")"),
  quotes   = c("`", "`"),
  dots = ".."
){
  if (is.function(x)){
    fmls <- names(formals(x))
    len_fmls <- length(fmls)

    if (len_fmls > 4){
      fmls <- fmls[1:4]
      fmls_fmt <- paste(fmls, collapse = ", ")
      fmls_fmt <- paste0(fmls_fmt, ", +", len_fmls - length(fmls), "")
    } else {
      fmls_fmt <- paste(fmls, collapse = ", ")
    }
    return(fmt_class(paste(
      fmt_class(class(x), open = "", close = ""), "(", fmls_fmt, ")",
      sep = ""
    )))
  }


  if (!is.atomic(x))
    return(class_fmt(x))

  if (is.numeric(x))
    x <- format(x, justify = "none", drop0trailing = TRUE, trim = TRUE)

  res <- ptrunc(x, collapse = ", ", width = width, dots = dots)

  if (length(x) > 1)
    res <- paste0(brackets[[1]], res, brackets[[2]])
  else
    res <- paste0(quotes[[1]], res, quotes[[2]])

  res
}




#' Collapse text vectors with a comma
#'
#' @param x `character` vector
#'
#' @return a `character` scalar
#' @noRd
comma <- function(..., collapse = ", "){
  paste(unlist(c(...)), collapse = collapse)
}




#' Collapse text vectors with a comma (no duplicates)
#'
#' @param x `character` vector
#'
#' @return a `character` scalar
#' @noRd
commaset <- function(..., collapse = ", "){
  paste(sort(unique(unlist(c(...)))), collapse = collapse)
}




#' Clean up paths to make them comparable, inspired by [fs::path_tidy()]
#'
#' @param x `character` vector
#'
#' @return a `character` vector
#' @noRd
path_tidy <- function(x){
  x <- gsub("\\\\", "/", x)
  x <- gsub("(?!^)/+", "/", x, perl = TRUE)

  sel <- x != "/"
  x[sel] <- gsub("/$", "", x[sel])

  sel <- is_windows_path(x)

  if (any(sel)){
    clean_win <- function(.x){
      substr(.x, 1, 1)  <- toupper(substr(.x, 1 ,1))
      .sel <- nchar(.x) == 2
      .x[.sel] <- paste0(.x[.sel], "/")
      .x
    }

    x[sel] <- clean_win(x[sel])
  }

  x
}




#' Clean up urls to make them comparable
#' @noRd
url_tidy <- function(...){
  sub("/", "//", gsub("/+", "/", paste(..., sep = "/")))
}




#' Return (unique) duplicated elements of a vector or rows of a data.frame
#'
#' For every element/row of `x` that has at least one duplicate, return one
#' instance of that element.
#'
#' @param x an [atomic] vector or [data.frame]
#' @param ... passed on to [duplicated()]
#'
#' @noRd
#'
#' @examples
#' dupes(c(1, 1, 1, 2))
#' dupes(cars[c(1, 1, 1, 2), ])
dupes <- function(x, ...){

  if (is.atomic(x)){
    sort(unique(x[duplicated(x, ...)]))
  } else if (is.data.frame(x)){
    res <- unique(x[duplicated(x, ...), ])
    row.names(res) <- NULL
    res
  }
}




#' Turn a character vector to camelCase
#'
#' **EXPERIMENTAL**
#'
#' @param x a `character` vector
#' @param sep_pattern a `regex` pattern to match separators
#'
#' @return a `character` vector that follows camelCase guidelines
#' @noRd
#'
#' @examples
#' camelCase("foo_bar")
camelCase <- function(x, sep_pattern = "_|\\s"){
  assert(is.character(x))
  assert(is_scalar_character(sep_pattern))
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))

  if (any(grepl(sep_pattern, x))){
    x <- gsub(paste0("^((", sep_pattern, ")*)|", "((", sep_pattern, ")*$)"), "", x)
    sep_positions <- gregexpr(sep_pattern, x)

    res <- vapply(
      seq_along(x),
      function(i_strings){
        string <- x[[i_strings]]
        i_seps <- sep_positions[[i_strings]]

        if (!identical(i_seps, -1L)){
          for (i_sep in i_seps){
            substr(string, i_sep + 1L, i_sep + 1L) <- toupper(substr(string, i_sep + 1L, i_sep + 1L))
          }
        }

        string
      },
      character(1)
    )
    res <- gsub(sep_pattern, "", res)
  } else {
    res <- x
  }

  res
}



#' Create description string for listlike R objects Rd documentation
#'
#' **EXPERIMENTAL**
#'
#' @param x any \R object
#' @noRd
rd_describe_str <- function(x){

  types <- paste0("`", vapply(x, class_fmt, character(1)), "`")

  cat(paste0("\\describe{\n",
    paste0("  \\item{", paste(names(x), types), "}{ }", collapse = "\n"),
    "\n}"))
}




#' Simple data validation
#'
#' **EXPERIMENTAL** Minimalistic mutli-criteria data validation. Designed to
#' work nicely together with `assert_all()` (see examples)
#'
#' @param ... Arbitrary expressions that evaluate to either `TRUE` or `FALSE`.
#'   Expressions that evaluate to anything else, will be counted as `FALSE` and
#'   throw a warning. You can name the expressions to generate nice labels
#'  (but that's usually not necessary).
#' @param .all `logical` scalar. Wrap each expression in `...` in `all()`
#'   (useful for validating columns in `data.frames`)
#'
#' @return a named `logical` vector that is guranteed to have no `NAs`.
#' @noRd
#'
#' @examples
#' validation <- validate(
#'   all(iris$Petal.Length < 5),
#'   `all species are valid` = all(iris$Species %in% c("setosa", "versicolor", "virginica")),
#'   `all sepals are small` = all(iris$Sepal.Length < 1)
#' )
#'
#' # validate can be used `with()` for added convenience:
#' validation <- with(iris, validate(
#'   all(Petal.Length < 5),
#'   `all species are valid` = all(Species %in% c("setosa", "versicolor", "virginica")),
#'   `all sepals are small` = all(Sepal.Length < 1)
#' ))
#'
#' # validate works together with assert_all to produce nice errors
#' try(
#'   assert_all(validation)
#' )
validate <- function(
    ...,
    .all = TRUE
){
  assert(is_scalar_bool(.all))
  expressions <- eval(substitute(alist(...)))

  result <- logical(length(expressions))
  criteria  <- character(length(expressions))
  criteria_names <- names(expressions)
  if (is.null(criteria_names)){
    criteria_names <- character(length(expressions))
  }


  for (i in seq_along(expressions)){
    criteria[[i]] <- paste(deparse(expressions[[i]], width.cutoff = 500L), collapse = " ")

    result[[i]] <-  tryCatch({
      res <- eval(expressions[[i]], envir = parent.frame(), enclos = parent.frame())
      if (.all){
        res <- all(res)
      }
      if (!is_scalar_bool(res)){
        stop("Cannot validate expression ", i, ": `", criteria[[i]], "` does not evaluate to either `TRUE` or `FALSE`", call. = FALSE)
      }
      res
    },

    error = function(e){
      warning(e$message, call. = FALSE)
      FALSE
    })
  }

  names(result) <- ifelse(is_blank(criteria_names), criteria, criteria_names)
  result
}




#' Assert all elements of a vector or list are `TRUE`
#'
#' Check if all elements of `x` are `TRUE`, and throw an informative warning that
#' contains the position and name (if any) of the element
#'
#' @param ... `logical` vectors or `lists` with only `logical` elements
#' @inheritParams assert
#'
#' @return `TRUE`
#' @seealso [assert()]
#' @noRd
#'
#' @examples
#' try(
#'   assert_all(c("this is true" = TRUE, "this is FALSE" = FALSE, FALSE))
#'   assert_all(
#'     list(a = c(TRUE, FALSE), b = TRUE),
#'     list(TRUE)
#'   )
#' )
assert_all <- function(
  ...,
  call = sys.call(-1)
){

  conds <- unlist(list(...))

  if (all(conds)){
    return(TRUE)
  }

  element_names <- names(conds)
  if (is.null(element_names)){
    element_names <- character(length(conds))
  }

  warning_messages <- character(0)

  for (i in seq_along(conds)){
    if (!isTRUE(conds[[i]])){
      if (is_blank(element_names[[i]])){
        warning_messages <- c(
          warning_messages,
          paste0("[", i, "] is not TRUE"))

      } else {
        warning_messages <- c(
          warning_messages,
          paste0("[", i, "] `", element_names[[i]], "` is not TRUE"))
      }
    }
  }

  error <- structure(
    list(
      message = paste0(c("Assertion failed", warning_messages), collapse = "\n"),
      call = call
    ),
    class = c("assertError", "error", "condition")
  )

  stop(error)
}

# nocov end
