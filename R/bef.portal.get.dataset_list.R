#' Fetch a list of datasets for a keyword
#'
#' This function fetches a list of datasets associated with a BEFdata portal keyword.
#'
#' @param keyword The keyword you like to fetch the associated datasets for
#' @param keyword_id This is the id of the keyword
#'
#' @examples \dontrun{
#'         list = bef.portal.get.dataset_list(keyword="carbon")
#'         list = bef.portal.get.dataset_list(keyword_id=47)
#'       }
#'
#' @import RCurl
#' @import rjson
#' @export

bef.portal.get.dataset_list <- function(keyword, keyword_id) {

  keyword_json=fromJSON(getURL(paste0(bef.options('url'),"/keywords.json")))
  names = unlist(lapply(keyword_json, function(x) (x$name)))
  ids = unlist(lapply(keyword_json, function(x) (x$id)))
  keyword_summary = data.frame(key = names, id = ids)


  if(!missing(keyword_id)) {
    url = keyword_url(id=keyword_id)
    full_url = paste(url, ".csv", sep = "")
    dataset_list = read.csv(full_url)
    output = data.frame(id=dataset_list$id, title=dataset_list$title)
    return(output)
  }

  if(!missing(keyword)) {
    position = grep(keyword, keyword_summary$key)
    get_keyword_id = keyword_summary$id[position]
    dataset_list = read.csv(paste(keyword_url(get_keyword_id),".csv", sep=""))
    output = data.frame(id=dataset_list$id, title=dataset_list$title)
    return(output)
  }
}
