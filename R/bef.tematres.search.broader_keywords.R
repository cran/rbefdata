#' Search a Tematres thesaurus via keywords (up hierarchy)
#'
#' @param lookup_keyword The keyword you are looking for
#'
#' @return The function returns a vector of keywords
#' @aliases bef.search.broader_keywords
#' @import RCurl
#' @import XML
#' @export

bef.tematres.search.broader_keywords <- bef.search.broader_keywords <-function(lookup_keyword)
  {
    service_task="search"
    service_argument=lookup_keyword
    service_url = sprintf("%s?task=%s&arg=%s", bef.options("tematres_service_url"), service_task, service_argument)

    search_fetch_xml=getURL(service_url)
    search_parse_xml=xmlTreeParse(search_fetch_xml, getDTD=F)
    search_get_xml_root=xmlRoot(search_parse_xml)

    if (length(grep("^result$",names(search_get_xml_root))) == 1)
      {
        search_get_results=search_get_xml_root[[1]]
        search_get_results_names=""

        for (i in 1:xmlSize(search_get_results)){
          search_get_results_names[i]=xmlSApply(search_get_xml_root[[1]][[i]][[2]], xmlValue)
        }

        if(length(grep(paste("^",service_argument,"$",sep=""),search_get_results_names,value=F)) == 1)
          {

            variable_index=which(search_get_results_names == lookup_keyword)
            get_id=xmlValue(search_get_xml_root[[1]][[variable_index]][[1]])

            service_task="fetchUp"
            service_argument=get_id

            fetch_upward_keywords=getURL(sprintf("%s?task=%s&arg=%s", bef.options("tematres_service_url"), service_task, service_argument))

            process_upward_keywords=xmlTreeParse(fetch_upward_keywords, getDTD=F)
            upward_root=xmlRoot(process_upward_keywords)

            upward_root_results=upward_root[[1]]

            upward_root_results_length=xmlSize(upward_root[[1]])
            upward_root_results_varnames=""

              for (j in 1:upward_root_results_length){
                upward_root_results_varnames[j]=xmlValue(upward_root_results[[j]][[2]][[1]])
              }

              return(upward_root_results_varnames)

          }
        }else{
          stop("No Terms available for your search")
      }
  }
