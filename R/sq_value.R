# sq_value <- function(value, quote = NULL){
#   #Check that value is suitable
#   if (inherits(value, "sq_value"))
#     return(value)
#   if (length(value) == 0)
#     stop("'value' must be of non-zero length")
#   if (all(is.na(value)))
#     stop("'value' must have at least one non-NA value")
#   if (!is_valid_vector(value))
#     stop("'value' must be an atomic vector of mode integer, numeric or character")
#
#   #value(s) are either for an IN clause or not
#   if (inherits(value,"IN") || length(value) > 1) {
#     value <- value[!is.na(value)]
#     if (inherits(value,"character")){
#       out <- DBI::dbQuoteString(DBI::ANSI(),value)
#     }else{
#       out <- value
#     }
#
#     out <- paste(as.character(out),collapse = ",")
#     out <- paste0("(", out, ")")
#   } else {
#     out <- dbi_interpolate(value,quote)
#   }
#
#   structure(as.character(out), class = c("sq_value", "character"))
# }

#' Prepare Value for an SQL Query
#'
#' @details \code{NA}s are dropped and at least one element of \code{value}
#' must be non-\code{NA}. Other values are coerced to their
#' character representation with \code{as.character}, and non-numeric and
#' non-NULL values are (single) quoted. Vectors of length >1 are
#' wrapped in parentheses and values are separated with commas. You can force
#' being wrapped in parenthese by using \code{IN}, e.g. \code{sq_value(IN(2))}.
#' The \code{quote} parameter is useful for dynamically specifying e.g. columns,
#' or table names.
#'
#' @param value A value or vector of values to be used in an SQL query.
#' @param quote The character to be used to quote the (non-numeric) value(s).
#' Can be one of '[', '"', "'" and "none". For brackets, use the opening bracket.
#' @return A character representation appropriate for SQL queries.
#' @export
sq_value <- function(value, quote = NULL){
  UseMethod("sq_value",value)
}

#' @export
sq_value.default <- function(value, quote = NULL){
  #Check that value is suitable
  if (inherits(value, "sq_value"))
    return(value)
  if (length(value) == 0)
    stop("'value' must be of non-zero length")
  if (all(is.na(value)))
    stop("'value' must have at least one non-NA value")
  if (!is_valid_vector(value))
    stop("'value' must be an atomic vector of mode integer, numeric or character")

  out <- dbi_interpolate(value,quote)
  structure(as.character(out), class = c("sq_value", "character"))
}

#' @export
sq_value.IN <- function(value, quote = FALSE){
  #Check that value is suitable
  if (inherits(value, "sq_value"))
    return(value)
  if (length(value) == 0)
    stop("'value' must be of non-zero length")
  if (all(is.na(value)))
    stop("'value' must have at least one non-NA value")
  if (!is_valid_vector(value))
    stop("'value' must be an atomic vector of mode integer, numeric or character")

  value <- value[!is.na(value)]
  if (inherits(value,"character")){
    out <- DBI::dbQuoteString(DBI::ANSI(),value)
  }else{
    out <- value
  }

  out <- paste(as.character(out),collapse = ",")
  out <- paste0("(", out, ")")

  structure(as.character(out), class = c("sq_value", "character"))
}

#' @export
sq_value.IS_NULL <- function(value,quore = FALSE){
  #Check that value is suitable
  if (inherits(value, "sq_value"))
    return(value)
  if (length(value) == 0)
    stop("'value' must be of non-zero length")
  if (all(is.na(value)))
    stop("'value' must have at least one non-NA value")
  if (!is_valid_vector(value))
    stop("'value' must be an atomic vector of mode integer, numeric or character")

  out <- dbi_interpolate(unclass(value),quote = "none")
  structure(as.character(out), class = c("sq_value", "character"))
}

#' @export
sq_value.IS_NOT_NULL <- sq_value.IS_NULL

is_valid_vector <- function(x){
  is.atomic(x) && mode(x) %in% c('integer','numeric','character')
}
