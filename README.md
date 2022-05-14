
# exceptions

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package is aimed at developers who want to throw meaningful and streamlined
exceptions from their R packages. The taxonomy of exceptions is heavily 
inspired (stolen) from python's built-in exception classes. See 
https://docs.python.org/3/library/exceptions.html. 

By providing proper exception classes from errors encountered by your functions,
you  makes it easier for users (and yourself) to respond to these errors 
in `tryCatch()` statements - for example, retry on a connection timeout, but 
not on a missing file. For for infos, please refer to the chapter on 
*Condition Handling* at http://adv-r.had.co.nz/Exceptions-Debugging.html


## Dependencies

This package depends only on base R. Additional packages are only required when 
converting errors produced by these same packages (rlang, httr, httr2) to
HTTP error objects, that can be serialized to json and (for example) returned  
by a plumber API.


## Installation

You can install the released version of exceptions from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("exceptions")
```

## Example

This is a basic example which shows you how to solve a common problem:

```
# without {exceptions}

stop("File 'command.com' not found")

# with {exceptions}

stop(exceptions::FileNotFoundError(file = "COMMAND.COM"))

```

