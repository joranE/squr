#' Parameterize an SQL Query
#'
#' @param .query An \code{sq} object or a character string.
#' @param ... name-value pairs of named parameters.
#'
#' @details The values will be prepared with \code{sq_value}
#'
#' @export
sq_set <- function(.query, ..., max_in = 3)
{
  dots <- list(...)
  dots_len <- sapply(dots,length)
  max_len <- max(dots_len)

  if (max_len > max_in){
    n_queries <- ceiling(max_len / max_in)
    .query <- replicate(n_queries,.query,simplify = FALSE)
    param_chunks <- lapply(seq_len(n_queries),
                           function(i,x) {lapply(x,`[[`,i)},
                           lapply(dots,el_split,n_chunks = n_queries))

    for (i in seq_along(.query)){
      params <- lapply(param_chunks[[i]], sq_value)
      names <- names(params)

      if (is.null(names) || any(names == ""))
        stop("Parameters must be named.")

      sort_index <- order(nchar(names))

      .query[[i]] <- sq_set_(.query[[i]], params[sort_index])
    }
    return(.query)

  } else{
    params <- lapply(dots, sq_value)
    names <- names(params)

    if (is.null(names) || any(names == ""))
      stop("Parameters must be named.")

    sort_index <- order(nchar(names))
    sq_set_(.query, params[sort_index])
  }
}

el_split <- function(x,n_chunks){
  if (length(x) == 1){
    old_class <- class(x)
    lapply(rep(x,n_chunks),`class<-`,old_class)
  }
  else{
    split(x,rep(seq_len(n_chunks),length.out = length(x)))
  }
}

#' Internal Recursive Parameterization Function
#'
#' @param query The query text to be parameterized.
#' @param params A list of parameters.
#'
#' @details The parameters should be sorted to have longer names first
#'   to avoid errors when some names are subsets of others.
#'
#' @noRd
sq_set_ <- function(query, params)
{
  param <- names(params)[[1L]]
  value <- params[[1L]]

  pattern <- paste0(param, "(?![[:alnum:]_#\\$\\@:])")

  prefix  <- `if`(any(grepl(paste0("@_", pattern), query, perl = TRUE)), "@_", "@")

  result <- gsub(paste0(prefix, pattern), value, query, perl = TRUE)

  if (length(params) > 1)
    sq_set_(result, params[-1])
  else
    sq_text(result)
}
