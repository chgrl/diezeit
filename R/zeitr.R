zeit_content <- function(query) {
  req <- zeit_get(path="content", q=query)
  raw <- zeit_parse(req)
  invisible(raw)
}
