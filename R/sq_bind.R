#' Bind values to parameters in SQL
#'
#' Insert values into SQL parameters. This is an internal function and in general
#' should not be called by the user. Instead, use \code{sq_set}.
#'
#' @param .query A sq object
#' @return A list, possibly of length greater than one, of SQL queries
#' @importFrom purrr pmap
#' @export
sq_bind <- function(.query){
  #If no parameters, nothing to do
  if (!has_params(.query)) return(.query$sql)
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

  #This prepares each value for insertion into the SQL text
  vals <- rapply(object = vals,
                 f = sq_value,
                 classes = c("integer","numeric","character","list"),
                 how = "replace")

  #This replicates the parameter values, if necessary, in case of split IN clauses
  # Works bc elements of vals are recycled as needed and only one element can
  # be of length >1
  vals_interp <- purrr::pmap(.l = vals,
                             .f = function(...) list(...))

  #Loop over copies of parameter values, inserting them into SQL text
  lapply(X = vals_interp,
         FUN = function(params,query) sq_bind_(query = query,params = params),
         query = .query$sql)
}

has_params <- function(.query){
  !is.null(.query$params) && length(.query$params) > 0
}

#' Internal Recursive Binding Function
#'
#' @param query The query text to be parameterized.
#' @param params A list of parameters.
#'
#' @details The parameters should be sorted to have longer names first
#'   to avoid errors when some names are subsets of others.
#'
sq_bind_ <- function(query, params){
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
    sq_bind_(result, params[-1])
  else
    structure(result,class = c("sq_sql","character"))
}
