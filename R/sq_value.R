#' Prepare Value for an SQL Query
#'
#' @details \code{NA}s are converted to NULL, other values are coerced to
#' character representation with \code{as.character}, and non-numeric and
#' non-NULL values are (single) quoted. Lists and vectors of length >1 are
#' wrapped in parentheses and values are separated with commas. You can force
#' being wrapped in parenthese by using \code{IN}, e.g. \code{sq_value(IN(2))}.
#' The \code{quote} parameter is useful for dynamically specifying e.g. columns,
#' or table names.
#'
#' @param value A value to be used in an SQL query.
#' @param quote The character to be used to quote the (non-numeric) value(s).
#' Can be one of '[', '"', "'" and "none". For brackets, use the opening bracket.
#' @return A character representation appropriate for SQL queries.
#'
#' @export
sq_value <- function(value, quote = NULL)
{
  if (inherits(value, "sq_value"))
    return(value)

  if (inherits(value,"IN") || length(value) > 1) {
    #Recursive call and collapse with commas
    #out <- paste(vapply(value, sq_value, character(1), quote = quote),
    #             collapse = ",")
    if (inherits(value,"character")){
      out <- DBI::dbQuoteString(DBI::ANSI(),value)
    }else{
      out <- value
    }
    out <- paste(as.character(out),collapse = ",")
    #Wrap in parens
    out <- paste0("(", out, ")")
  } else {
    out <- rep("NULL", length(value))
    available <- !is.na(value)

    if (any(available)) {
      if (is.numeric(value)) {
        out[available] <- as.character(value[available])
      } else {
        out[available] <- dbi_interpolate(as.character(value[available]), quote)
      }
    }
  }
  structure(out, class = c("sq_value", "character"))
}
