#' Inline SQL Query
#'
#' @param .query A character SQL query.
#'
#' @return An \code{sq} object.
#'
#' @export
sq_text <- function(.query)
{
  structure(.query$sql, class = c("sq", "character"),
            docs = .query$doc)
}
