#' Read a SQL File
#'
#' @param path The path to the file to be read.
#'
#' @return List with elements \code{sql} containing the SQL text and \code{docs}
#' containing any documention lines beginning with "--".
read_sql_file <- function(path){
  content <- readLines(path, warn = FALSE)

  #Grab only first consecutive run of commented lines
  idx <- rle(grepl("^--",content))
  if (!idx$values[1]){
    #No docs
    query_docs <- character(0)
  }else{
    query_docs <- content[seq_len(idx$lengths[1])]
  }
  query_text <- content[!grepl("^--",content)]
  query_text <- paste(query_text, collapse = "\n")

  return(list(sql = query_text,
              docs = query_docs))
}
