#' @title Observe your usage
#' @description \code{zeit_client} does not provide content per se, 
#' but lets you get information about your API usage.
#'
#' @return a list of information about the client and API usage
#' @export
#' @examples
#' \dontrun{
#' zeit_client()
#' }
zeit_client <- function() {
	# make request
  req <- zeit_get(path="client")
  raw <- zeit_parse(req)
  #reset <- as.POSIXct(raw$reset, origin="1970-01-01")
  return(raw)
}


#' @title Search the ZEIT archive
#' @description \code{zeit_search} exposes a search for ZEIT archive items. 
#' You can set search queries, paginate, sort and partially select the fields, 
#' that should be returned. Articles, that match your query, are returned with
#' a reduced set of meta data.
#'
#' @param endpoint one of \code{author}, \code{content}, \code{department}, \code{keyword}, 
#' \code{product} or \code{series} -- provides specific search functionalities.
#' @param query the main search query; single character value or character vector.
#' @param fields partially select output fields; set to \code{"all"} by default.
#' @param limit limit the amount of matches to return; set to 10 by default.
#' @param offset offset for the list of matches; set to 0 by default.
#' @return a list of matches to the query.
#' @export
#' @examples
#' \dontrun{
#' zeit_search("")
#' }
zeit_search <- function(endpoint, query, fields="all", limit=10, offset=0) {
	# prepare endpoint
	avail.endpoints <- c("author", "content", "department", "keyword", "product", "series")
	endpoint <- avail.endpoints[pmatch(endpoint, avail.endpoints)]
	
	# prepare query
	query <- paste0(sapply(query, function(x) gsub(" ", "%20", x, fixed=TRUE)), collapse="+")
	
	# prepare fields
	if(endpoint == "author") avail.fields <- c("href", "id", "type", "uri", "value")
	else if(endpoint == "content") avail.fields <- c("subtitle", "uuid", "title", "href", "release_date", "uri", "snippet", "supertitle", "teaser_text", "teaser_title")
	else if(endpoint == "department") avail.fields <- c("uri", "value", "parent", "href")
	else if(endpoint == "keyword") avail.fields <- c("uri", "value", "lexical", "type", "score", "href")
	else if(endpoint == "product") avail.fields <- c("uri", "value")
	else if(endpoint == "series") avail.fields <- c("uri", "value", "href")
  if(fields!="all") fields <- avail.fields[pmatch(fields, avail.fields)]
  if(length(fields)>1) fields <- paste0(fields, collapse=",")
	
	# make request
  if(fields=="all") req <- zeit_get(path=endpoint, q=query, limit=limit, offset=offset)
  else req <- zeit_get(path=endpoint, q=query, fields=fields, limit=limit, offset=offset)
  raw <- zeit_parse(req)
  return(raw)
}