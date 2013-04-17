#' Search a Tematres thesaurus via keywords (down hierarchy)
#'
#' @param lookup_keyword The keyword you are looking for
#'
#' @return The function returns a vector of keywords
#' @aliases bef.search.narrower_keywords
#' @import RCurl
#' @import XML
#' @export

bef.tematres.search.narrower_keywords <- bef.search.narrower_keywords <- function(lookup_keyword)
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
      } else {
        stop("The server says: No terms available for your search")
      }

    variable_index=grep(paste("^",service_argument,"$","|","^",service_argument,"s","$", sep=""), search_get_results_names, ignore.case = T)
    get_id=xmlValue(search_get_xml_root[[1]][[variable_index]][[1]])

    service_task="fetchDown"
    service_argument=get_id

    fetch_upward_keywords=getURL(sprintf("%s?task=%s&arg=%s", bef.options("tematres_service_url"), service_task, service_argument))

    process_upward_keywords=xmlTreeParse(fetch_upward_keywords, getDTD=F)

    if (length(grep("^result$",names(process_upward_keywords[[1]]))) == 1)
      {
        upward_root=xmlRoot(process_upward_keywords)
        upward_root_results=upward_root[[1]]
        upward_root_results_length=xmlSize(upward_root[[1]])
        upward_root_results_varnames=""

        for (j in 1:upward_root_results_length)
          {
            upward_root_results_varnames[j]=xmlValue(upward_root_results[[j]][[2]][[1]])
          }

        print(upward_root_results_varnames)
    }
  }
