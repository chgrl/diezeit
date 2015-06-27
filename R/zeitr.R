zeit_content <- function(query, fields=NA, limit=10, offset=0) {
	# prepare query
	if(length(query)==1) query <- gsub(" ", "%20", query, fixed=TRUE)
	else query <- paste0(query, collapse="+")
	
	# check fields
	avail.fields <- c("subtitle", "uuid", "title", "href", "release_date", "uri", "snippet", "supertitle", "teaser_text", "teaser_title")
  if(!is.na(fields)) fields <- avail.fields[pmatch(fields, avail.fields)]
	
	# make request
  if(is.na(fields)) req <- zeit_get(path="content", q=query, limit=limit, offset=offset)
  req <- zeit_get(path="content", q=query, fields=fields, limit=limit, offset=offset)
  raw <- zeit_parse(req)
  invisible(raw)
}
