#' Wrapper for DBIs sqlInterpolate
#'
#' This function uses `sqlInterpolate` to parse character values.
#' The quote argument controls how the value is quoted, if at all.
#'
#' @param value The value to be quoted.
#' @param quote Optional quoting character to use; can be one of '[', '"', "'"
#' or "none".
#' @importFrom DBI sqlInterpolate ANSI
dbi_interpolate <- function(value, quote = NULL){
  if (length(value) > 1) {
    purrr::map_chr(value, dbi_interpolate)
  } else {

    right_quote <-
      if (!is.null(quote)) switch(quote, "[" = "]", '"' = '"', "'" = "'","none" = "",
                                  stop("Invalid quote character."))

    out <- DBI::sqlInterpolate(ANSI(), "?value", value = value)

    if (!is.null(quote)) {
      left_quote <- if (quote == "none") "" else quote
      gsub("^[^[:alnum:]]", left_quote, gsub("[^[:alnum:]]$", right_quote, out))
    } else {
      out
    }
  }
}
