#' Bind values to parameters in SQL
#'
#' Insert values into SQL parameters.
#'
#' @param .query A sq object
#' @return A list, possibly of length greater than one, of SQL queries
#' @importFrom purrr pmap
#' @export
sq_bind <- function(.query){
  #If no parameters, nothing to do
  if (!has_params(.query)) return(.query)
  #Parameters but no values is an error
  if (has_params(.query) && is.null(.query$values))
    stop(".query has parameters but no values to bind to them.")

  vals <- .query$values

  #Check if >1 value vector was split for being longer than max_in
  vals_len <- vapply(X = vals,
                     FUN = function(x) if (!is.list(x)) 1L else length(x),
                     FUN.VALUE = integer(1))
  if (sum(vals_len > 1) > 1){
    stop("Only allowed 1 param value with length >1. Cannot split query across >1 IN clause.")
  }

  vals <- rapply(object = vals,
                 f = sq_value,
                 classes = c("integer","numeric","character","list"),
                 how = "replace")
  vals_interp <- purrr::pmap(.l = vals,
                             .f = function(...) list(...))

  lapply(X = vals_interp,
         FUN = function(params,query) sq_set_(query = query,params = params),
         query = .query$sql)
}

has_params <- function(.query){
  !is.null(.query$params) && length(.query$params) > 0
}
