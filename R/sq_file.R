#' Read the contents of an SQL file
#'
#' Read query from a .sql file. Parameters in the query should be prefixed
#' with "@". This function automatically detects whether it has been called
#' from within a package (rather than interactively by the user) and adjusts
#' the file path accordingly. The argument \code{override_pkg} can be used
#' to bypass this automatic behavior and force the use of the raw file path
#' even when called inside a package.
#'
#' @param path character specifying the path to an SQL file. The ".sql"
#' extension can be omitted if and only if the actual extension is lower case.
#' When used in a package, the path will be taken to be relative to the
#' \code{inst} folder.
#' @param override_pkg boolean if \code{TRUE} then forces the use of the raw path
#' rather than looking inside the package installation.
#'
#' @return A \code{sq} object; a list with components \code{sql}, \code{params},
#' \code{values} and \code{docs}.
#'
#' @details \code{sq_file} assumes that any lines at the beginning of the SQL file
#' that are commented out via "- -" (two dashes) contain documentation. Subsequent commented out
#' lines within the query itself are not considered part of the SQL file documentation.
#'
#' It assumes a simple format roughly in the style of roxygen2. The first line
#' contains the query title, followed by a blank comment line, and then possibly
#' several lines comprising a description of the query.
#'
#' Next, \code{sq_file} recognizes three tags:
#'
#' \describe{
#' \item{@@param}{the name of the parameter followed by a space and then a description;
#' you can include multiple param tags each starting on its own line}
#' \item{@@functions}{comma separated list of functions that use this query}
#' \item{@@scripts}{comma separated list of scripts that use this query}
#' }
#'
#' @export
sq_file <- function(path,override_pkg = FALSE){
  if (!is_scalar_character(path))
    stop("Argument 'path' should be a scalar character value")

  path.sql <- append_sql_extension(path)

  if (is_packaged() && !override_pkg) {
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

