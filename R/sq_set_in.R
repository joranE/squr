#' @export
sq_set_in <- function(.query,...,max_in = 5){
  #Collect IN param and check lengths
  # Params with length > 1 are assumed to be
  # for IN clauses
  dots <- list(...)
  if (length(dots) > 1) stop("Can only split on one IN clause param at a time.")
  dots_len <- sapply(dots,length)
  max_len <- max(dots_len)

  #Query needs to be split
  if (max_len > max_in){
    n_queries <- ceiling(max_len / max_in)
    .query <- replicate(n_queries,.query,simplify = FALSE)
    param_chunks <- lapply(seq_len(n_queries),
                           function(i,x) {lapply(x,`[[`,i)},
                           lapply(dots,split_params,n_chunks = n_queries))

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

split_params <- function(x,n_chunks){
  if (length(x) == 1){
    old_class <- class(x)
    lapply(rep(x,n_chunks),`class<-`,old_class)
  }
  else{
    split(x,rep(seq_len(n_chunks),length.out = length(x)))
  }
}
