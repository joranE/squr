#' S3 Print Method for 'sq' Objects
#'
#' @param x an sq object to be printed.
#' @param ... Passed on to \code{cat}.
#'
#' @export
print.sq <- function(x, ...){
  if (!inherits(x, "sq") || !is.character(x$sql))
    stop("Invalid sq object.")

  cat(x$sql)
  invisible(x)
}

#' @export
print.sql <- function(x, ...){
  if (!inherits(x, "sql") || !is.character(x))
    stop("Invalid sql object.")

  cat(x)
  invisible(x)
}

#' S3 Print Method for 'sq_value' Objects
#'
#' @param x an sq_value object to be printed
#' @param ... Passed to \code{print.default}.
#'
#' @export
print.sq_value <- function(x, ...){
  if (!inherits(x, "sq_value") || !is.character(x))
    stop("Invalid sq_value object.")

  cat("sq_value:\n")

  print(unclass(x))
  invisible(x)
}
