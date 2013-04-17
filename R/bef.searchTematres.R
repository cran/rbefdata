#' Search a Tematres thesaurus via keywords
#'
#' @param lookup_keyword The keyword you are looking for
#'
#' @return The function returns a vector of keywords
#' @import RCurl
#' @import XML
#' @export

bef.searchTematres <- function(lookup_keyword)
  {
    service_task="search"
    service_argument=lookup_keyword
    service_url = sprintf("%s?task=%s&arg=%s", bef.options("tematres_service_url"), service_task, service_argument)

    search_fetch_xml=getURL(service_url)
    search_parse_xml=xmlTreeParse(search_fetch_xml, getDTD=F)
    search_get_xml_root = xmlRoot(search_parse_xml)

    if (length(grep("^result$",names(search_get_xml_root))) == 1)
      {
        search_get_results=search_get_xml_root[[1]]
        search_get_results_names=""

        for (i in 1:xmlSize(search_get_results))
          {
            search_get_results_names[i]=xmlSApply(search_get_xml_root[[1]][[i]][[2]], xmlValue)
          }

        return(search_get_results_names)

      }else{
        stop("The server says: No terms available for your search")
      }
  }
