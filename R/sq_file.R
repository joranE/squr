#' Read The Contents of an SQL File
#'
#' @param path character specifying the path to an SQL file. The ".sql"
#' extension can be omitted if and only if the actual extension is lower case.
#' When used in a package, the path will be taken to be relative to the
#' \code{inst} folder.
#'
#' @return A \code{sq} object; a list with components \code{sql}, \code{params},
#' \code{values} and \code{docs}.
#'
#' @export
sq_file <- function(path,remove_ignored = TRUE){
  if (!is_scalar_character(path))
    stop("Argument 'path' should be a scalar character value")

  path.sql <- append_sql_extension(path)

  if (is_packaged()) {
    pkg_name <- package_name()
    use_path <- system.file(path.sql, package = pkg_name)
    if (use_path == "")
      stop(sprintf("The SQL file '%s' cannot be found in package '%s'",
                   path.sql,
                   pkg_name))

  } else {
    use_path <- path.sql
  }

  normalized <- normalizePath(use_path, mustWork = FALSE)

  if (!file.exists(normalized))
    stop(sprintf("The SQL file '%s' cannot be found.", normalized))

  sql <- read_sql_file(normalized)

  structure(list(sql = sql$sql,
                 params = get_params(.query = sql$sql),
                 values = NULL,
                 docs = sql$docs),
            class = "sq")
}

