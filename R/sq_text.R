#' Inline SQL Query
#'
#' Create a `sq` object from inline SQL passed as a character object.
#'
#' @param .query A character SQL query.
#'
#' @return An \code{sq} object.
#'
#' @export
sq_text <- function(.query){
  structure(list(sql = .query,
                 params = get_params(.query = .query),
                 values = NULL,
                 docs = NULL),
            class = "sq")
}
