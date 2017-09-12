#' Parses the query and returns a list with all the elements of the comment.
#'
#' @param query_docs the query name.
#' @return a list with documentation including \code{introduction}, \code{return},
#'         and \code{params} (as a data frame).
sq_docs <- function(.query) {
  lines <- attr(.query,"doc")

  #Borrowed heavily from roxygen2
  #https://github.com/yihui/roxygen2/blob/master/R/parse-preref.R
  LINE.DELIMITER <- "\\s*#+' ?"
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""), "right")
  if (length(trimmed.lines) == 0) return(list())
  joined.lines <- str_c(trimmed.lines, collapse = '\n')
  elements <- strsplit(joined.lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]
  elements <- str_replace_all(elements, fixed("@@"), "@")
  parsed.introduction <- parse_instruduction(elements[[1]])
  parsed.elements <- unlist(lapply(elements[-1], parse_element), recursive = FALSE)

  sqldoc <- c(parsed.introduction, parsed.elements)

  if(length(get_params(.query)) > 0 & !is.na(get_params(.query)[1])) {

    params <- data.frame(Param = get_params(.query),
                         Desc = NA,
                         stringsAsFactors=FALSE)

    for(l in sqldoc[names(sqldoc) == 'param']) {
      params[params$Param == l$name,]$Desc <- l$description
    }

    for(l in sqldoc[names(sqldoc) == 'default']) {
      params[params$Param == l$name,]$default <- l$description
    }
    sqldoc$params <- params
  }

  returns <- data.frame(Variable = character(),
                        Desc = character(),
                        stringsAsFactors=FALSE)

  for(l in sqldoc[names(sqldoc) == 'return']) {
    returns <- rbind(returns,
                     data.frame(Variable = l$name,
                                Desc=l$description,
                                stringsAsFactors=FALSE))
  }

  sqldoc <- sqldoc[!(names(sqldoc) %in% c('param', 'return'))]
  sqldoc$returns <- returns

  class(sqldoc) <- c('sq_doc')
  return(sqldoc)
}

#' Prints the SQL documentation.
#' @param x sq_doc object.
#' @param ... currently unused.
#' @method print sq_doc
#' @rdname print
print.sq_doc <- function(x, ...) {
  cat(x$introduction)
  cat('\n\n')
  if(!is.null(x$params)) {
    cat('Parameters:\n')
    print(x$params, row.names=FALSE)
  }
  cat("\n")
  if(!is.null(x$returns)) {
    cat('Returns (note that this list may not be complete):\n')
    print(x$returns, row.names=FALSE)
  }
}

#' Parse a raw string containing key and expressions.
#'
#' Copied from roxygen2: https://github.com/yihui/roxygen2/blob/master/R/parse-preref.R
#'
#' @param element the string containing key and expressions
#' @param srcref source reference.
#' @return A list containing the parsed constituents
#' @author yihui
parse_element <- function(element, srcref) {
  # From an old version of roxygen2
  parse_name_description <- function(key, rest, srcref) {
    pieces <- str_split_fixed(rest, "[[:space:]]+", 2)
    name <- pieces[, 1]
    rest <- str_trim(pieces[, 2])
    if(is_null_string(name)) {
      stop(paste0(key, " requires a name and description: ", srcref))
    }
    list(name = name, description = rest)
  }

  #TODO: This should only be done once when the package loads
  preref_parsers <- new.env(parent=emptyenv())
  preref_parsers[['default']] <- parse_name_description
  preref_parsers[['return']] <- parse_name_description
  preref_parsers[['param']] <- parse_name_description

  pieces <- str_split_fixed(element, "[[:space:]]+", 2)

  tag <- pieces[, 1]
  rest <- pieces[, 2]

  #tag_parser <- preref_parsers[[tag]] %||% parse.unknown
  tag_parser <- preref_parsers[[tag]]
  res <- list(tag_parser(tag, rest, NULL))
  names(res) <- tag
  return(res)
}

#' Parse introduction: the premier part of a roxygen block
#' containing description and option details separated by
#' a blank roxygen line.
#'
#' Copied from roxygen2: https://github.com/yihui/roxygen2/blob/master/R/parse-preref.R
#'
#' @param expression the description to be parsed
#' @return A list containing the parsed description
#' @author yihui
parse_instruduction <- function(expression) {
  if (is_null_string(expression)) return(NULL)
  list(introduction = str_trim(expression))
}

#' Does the string contain no matter, but very well [:space:]?
#' @param string the string to check
#' @return TRUE if the string contains words, otherwise FALSE
is_null_string <- function(string) {
  str_length(str_trim(string)) == 0
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
