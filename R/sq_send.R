#' Send a query
#'
#' Small function that invokes another given function for sending
#' a query to the database. The benefit of using this wrapper is
#' to keep database/driver boilerplate code separate from functions
#' that sends queries.
#'
#' Values are bound to parameters here via \code{sq_bind}. If necessary,
#' long IN clause values are split, multiple identical queries are sent and
#' then the results are combined.
#'
#' @param .query A character/sq query string.
#' @param .with function with which to send the query.
#' @param ... further parameters passed to \code{with}.
#'
#' @return The value returned by \code{.with}
#' @export
sq_send <- function(.query, .with, ...){
  bound_query <- sq_bind(.query)
  results <- lapply(bound_query,FUN = .with,...)
  results <- do.call("rbind",results)
  rownames(results) <- NULL
  results
}
