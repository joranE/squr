#' Parameterize an SQL Query
#'
#' Assign values to parameters in the query. The SQL text is not altered in this
#' function, the values are merely recorded. Binding occurs at the last moment
#' in \code{sq_send}.
#'
#' @param .query An \code{sq} object or a character string.
#' @param ... name-value pairs of named parameters. Unnamed arguments or named
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


    if (inherits(dots[[i]],"IS_NULL") || inherits(dots[[i]],"IS_NOT_NULL")){
      regex <- sprintf("([=<>]{1,3}|(like)|(LIKE))\\s+(%s)",paste0("@",dot_names[i]))
      .query$sql <- gsub(pattern = regex,
                         replacement = "\\1",
                         .query$sql)
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
IN <- function(x = NULL){
  if (is.null(x)) stop("Argument x passed to IN() is NULL")
  structure(x,class = c("IN",class(x)))
}

#' Pad values with wildcard symbol
#'
#' Pads single values with wildcard symbol on left, right or both
#' for use with parameters in LIKE expressions. Used when you have something
#' like \code{column like @param} in your sql file in order to convert it to
#' \code{column like '%val'}, \code{column like 'val%'} or \code{column like '%val%}.
#'
#' @param x character
#' @param side character; one of 'l' (left), 'r' (right) or 'b' (both)
#' @param wildcard character; defaults to "\%"
#' @export
LIKE <- function(x = NULL,side,wildcard = "%"){
  if (is.null(x)) stop("Argument x passed to LIKE() is NULL")
  if (!side %in% c('l','r','b')) stop("side must be 'l', 'r' or 'b'.")
  if (length(x) > 1) stop("Argument x should be of length one.")

  switch(side,
         'l' = paste0(wildcard,x),
         'r' = paste0(x,wildcard),
         'b' = paste0(wildcard,x,wildcard))
}

#' Flag parameter to be set to NULL
#'
#' Called with no arguments, this flags a parameter for conversion to NULL,
#' i.e. from something like \code{column <= @param} to \code{column is null}.
#'
#' @export
IS_NULL <- function(){
  structure("is null",class = c("IS_NULL","character"))
}

#' Flag parameter to be set to not NULL
#'
#' Called with no arguments, this flags a parameter for conversion to NULL,
#' i.e. from something like \code{column <= @param} to \code{column is not null}.
#' @export
IS_NOT_NULL <- function(){
  structure("is not null",class = c("IS_NOT_NULL","character"))
}
