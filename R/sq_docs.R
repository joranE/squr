#' @export
sq_parse_docs <- function(.query){
  docs <- gsub(pattern = "^--",
               replacement = "",
               x = .query$docs)
  docs <- trimws(docs,which = "both")

  tag_idx <- which(grepl("^@",docs))

  title <- docs[1]
  description <- docs[2:(tag_idx[1] - 1)]
  description <- trimws(paste(description,collapse = " "),which = "both")

  tags <- vector(mode = "list",length = length(tag_idx))
  tag_names <- vector(mode = "list",length = length(tag_idx))

  for (i in seq_along(tag_idx)){
    if (i == length(tag_idx)){
      tags[[i]] <- docs[tag_idx[i]:length(docs)]
    }else{
      tags[[i]] <- docs[tag_idx[i]:(tag_idx[i+1] - 1)]
    }
    tags[[i]] <- paste(tags[[i]],collapse = " ")
    tag_names[[i]] <- stringr::str_extract(tags[[i]],
                                           pattern = "^@(param)|^@(functions)|^@(scripts)")
    tags[[i]] <- stringr::str_replace(string = tags[[i]],
                                      pattern = "^@(param)\\s+|^@(functions)\\s+|^@(scripts)\\s+",
                                      replacement = "")
    if (tag_names[[i]] == '@param'){
      rexpr <- "^(\\w+)\\s?(.*)$"
      tags[[i]] <- list(param_name = sub(rexpr,"\\1",tags[[i]]),
                        param_desc = sub(rexpr,"\\2",tags[[i]]))
    }

    if (tag_names[[i]] %in% c('@functions','@scripts')){
      tags[[i]] <- strsplit(tags[[i]],",\\s+")
    }

  }

  names(tags) <- gsub(pattern = "@","",tag_names,fixed = TRUE)

  title_chunk <- "---\ntitle: '%s'\noutput: html_document\n---\n"
  description_chunk <- "### Description\n\n%s\n"
  param_chunk <- "### Parameters\n\n%s"
  functions_chunk <- "### Functions\nThis query is called from the following functions:\n\n%s"
  scripts_chunk <- "### Scripts\nThis query is called from the following scripts:\n\n%s"
  sql_chunk <- sprintf("### SQL\n```{sql}\n%s\n```\n",.query$sql)

  param_docs <- tags[grepl(pattern = "param",names(tags))]
  fun_docs <- tags[grepl(pattern = "functions",names(tags))]
  script_docs <- tags[grepl(pattern = "scripts",names(tags))]

  param_docs <- lapply(param_docs,
                       function(x) paste("*",paste(x,collapse = " - "),sep = " "))
  param_docs <- paste(param_docs,collapse = "\n")
  param_chunk <- sprintf(param_chunk,param_docs)

  fun_docs <- lapply(unlist(fun_docs),
                     function(x) paste(paste("*",x),collapse = "\n"))
  functions_chunk <- sprintf(functions_chunk,paste(fun_docs,collapse = "\n"))

  script_docs <- lapply(unlist(script_docs),
                        function(x) paste(paste("*",x),collapse = "\n"))
  scripts_chunk <- sprintf(scripts_chunk,paste(script_docs,collapse = "\n"))

  structure(list(title = sprintf(title_chunk,title),
              description = sprintf(description_chunk,description),
              parameters = paste0(param_chunk,"\n"),
              functions = paste0(functions_chunk,"\n"),
              scripts = paste0(scripts_chunk,"\n"),
              sql = sql_chunk),
            class = "sq_docs")
}

#' @export
#' @importFrom rmarkdown render
#' @importFrom rstudioapi viewer
sq_view_docs <- function(docs){
  dir <- tempfile()
  dir.create(dir)
  md_file <- file.path(dir, "sq_doc.md")

  for (i in seq_along(docs)){
    cat(docs[[i]],
        sep = "\n",
        file = md_file,
        fill = FALSE,
        append = TRUE)
  }

  rmarkdown::render(input = md_file,
                    output_file = "sq_doc.html",
                    quiet = TRUE)

  rstudioapi::viewer(file.path(dir,"sq_doc.html"))
}
