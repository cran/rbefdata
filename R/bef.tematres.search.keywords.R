#' Search a Tematres thesaurus via keywords
#'
#' @param lookup_keyword The keyword you are looking for
#'
#' @return The function returns a vector of keywords
#' @aliases bef.search.keywords
#' @import RCurl
#' @import XML
#' @export

bef.tematres.search.keywords <- bef.search.keywords <- function(lookup_keyword) {
  service_task = "search"
  service_argument = lookup_keyword
  service_url = sprintf("%s?task=%s&arg=%s", bef.options("tematres_service_url"), service_task, service_argument)
  search_fetch_xml = xmlParse(service_url)
  items = xpathSApply(search_fetch_xml, path = "//result/term/string", xmlValue, ignoreComments=T)
  if (length(items)) {
    ids = xpathSApply(search_fetch_xml, path = "//result/term/term_id", xmlValue, ignoreComments=T)
    attr(items, "id") = as.numeric(ids)
  }
  else {
    stop("The server says: No terms available for your search")
  }
  return(items)
}
