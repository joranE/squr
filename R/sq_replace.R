#' Replace arbitary parts of query using gsub
#'
#' Thin wrapper around \code{gsub} for manual edits to SQL text. Can only be
#' used prior setting parameter values with `\code{sq_set}.
#'
#' @param .query a sq object
#' @param pattern character; passed to pattern argument of gsub
#' @param replacement character; passed to replacement argument of gsub
#' @param \dots further arguments to gsub
#'
#' @return A sq object with possibly modified set of parameters.
#' @export
sq_replace <- function(.query,pattern,replacement,...){
  if (!is.null(.query$values))
    stop("Cannot edit SQL text once param values have been set.")

  .query$sql <- gsub(pattern = pattern,
                     replacement = replacement,
                     x = .query$sql,...)
  .query$params <- get_params(.query$sql)
  .query
}
