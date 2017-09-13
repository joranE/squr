#' Parameterize an SQL Query
#'
#' Assign values to parameters in the query. The SQL text is not altered in this
#' function, the values are merely recorded. Binding occurs at the last moment
#' in \code{sq_send}.
#'
#' @param .query An \code{sq} object or a character string.
#' @param ... name-value pairs of named parameters. Unnamed arguemtns or named
#' arguments that do not match any parameters in the query will generate an
#' error. Vectors intended to be used as values for \code{IN} clauses should be
#' passed via the \code{IN} function, e.g. \code{name = IN(value)}. Such values
#' with length greater than \code{max_in} will be split into pieces of size at
#' most \code{max_in}. For parameters corresponding to column or table names you
#' can use \code{sq_value} directly in order to specify the correct quoting behavior.
#' @param max_in maximum number of values allowed in a single IN clause. Defaults
#' to 1000.
#'
#' @details The values will be prepared with \code{sq_value}
#' @return A sq object with its \code{values} element set.
#' @export
sq_set <- function(.query, ..., max_in = 1e3){
  dots <- list(...)
  dot_names <- names(dots)

  if (is.null(dot_names) || any(dot_names == "")){
    stop("Parameters must be named.")
  }

  missing_params <- setdiff(names(dots),.query$params)
  if (length(missing_params) > 0){
    stop(sprintf("Parameter(s) %s not found in SQL text.",
                 paste(missing_params,collapse = ",")))
  }

  for (i in seq_along(dots)){
    n <- length(dots[[i]])
    if (n > max_in){
      #To preserve IN class in case one piece is a singleton
      old_class <- class(dots[[i]])
      dots[[i]] <- split(x = dots[[i]],
                         f = ntile(dots[[i]],ceiling(n / max_in)))
      dots[[i]] <- lapply(dots[[i]],`class<-`,old_class)
    }
  }

  if (is.null(.query$values)){
    .query$values <- dots
  }else{
    extra_params <- setdiff(names(.query$values),names(dots))
    .query$values <- c(.query$values[extra_params],dots)
  }
  .query
}

#' Internal Recursive Parameterization Function
#'
#' @param query The query text to be parameterized.
#' @param params A list of parameters.
#'
#' @details The parameters should be sorted to have longer names first
#'   to avoid errors when some names are subsets of others.
#'
sq_set_ <- function(query, params){
  param <- names(params)[[1L]]
  value <- params[[1L]]

  pattern <- paste0(param, "(?![[:alnum:]_#\\$\\@:])")

  if (any(grepl(paste0("@_", pattern), query, perl = TRUE))){
    prefix <- "@_"
  }else{
    prefix <- "@"
  }

  result <- gsub(paste0(prefix, pattern), value, query, perl = TRUE)

  if (length(params) > 1)
    sq_set_(result, params[-1])
  else
    structure(result,class = c("sql","character"))
}

#' Rough binning rank
#'
#' Borrowed from dplyr::ntile.
#'
#' @param x vector
#' @param n number of bins
ntile <- function (x, n){
  len <- sum(!is.na(x))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  }
  else {
    as.integer(floor(n * (rank(x,ties.method = "first",na.last = "keep") - 1)/len + 1))
  }
}

#' Mark parameter value for use with IN clause
#'
#' @param x a vector of values, possibly only of length 1
#' @return The original object with the class attribute appended with "IN".
#' @export
IN <- function(x){
  structure(x,class = c(class(x),"IN"))
}
