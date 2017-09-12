#' Get Parameter Names
#'
#' Retrieve parameter names from SQL text. Assumes all parameters are of the
#' form "@param".
#'
#' @param .query SQL text
#' @importFrom stringr str_extract_all
#' @export
get_params <- function(.query){
  params <- lapply(.query,function(x) stringr::str_extract_all(x,"\\B\\@\\w+"))
  gsub("@","",unique(unlist(params)),fixed = TRUE)
}
