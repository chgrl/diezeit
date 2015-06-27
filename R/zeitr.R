zeit_client <- function() {
	# make request
  req <- zeit_get(path="client")
  raw <- zeit_parse(req)
  #reset <- as.POSIXct(raw$reset, origin="1970-01-01")
  invisible(raw)
}


zeit_content <- function(query, fields="all", limit=10, offset=0) {
	# prepare query
	query <- paste0(sapply(query, function(x) gsub(" ", "%20", x, fixed=TRUE)), collapse="+")
	str(query)
	# prepare fields
	avail.fields <- c("subtitle", "uuid", "title", "href", "release_date", "uri", "snippet", "supertitle", "teaser_text", "teaser_title")
  if(fields!="all") fields <- avail.fields[pmatch(fields, avail.fields)]
  if(length(fields)>1) fields <- paste0(fields, collapse=",")
	
	# make request
  if(fields=="all") req <- zeit_get(path="content", q=query, limit=limit, offset=offset)
  else req <- zeit_get(path="content", q=query, fields=fields, limit=limit, offset=offset)
  raw <- zeit_parse(req)
  return(raw)
}
