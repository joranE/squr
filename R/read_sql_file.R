#' Read a SQL File
#'
#' @param path The path to the file to be read.
#'
#' @return List with elements \code{sql} containing the SQL text and \code{docs}
#' containing any documention lines beginning with #'.
#' @noRd
read_sql_file <- function(path){
  content <- readLines(path, warn = FALSE)
  query_docs <- content[grepl("^#'",content)]
  query_text <- content[!grepl("^#'",content)]
  query_text <- paste(query_text, collapse = "\n")

  return(list(sql = query_text,
              docs = query_docs))
}
