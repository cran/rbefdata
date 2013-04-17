#' A wrapper for the search on the tematres thesaurus.
#'
#' Search a Tematres thesaurus via keywords. This function is a wrapper and so it calls
#' the appropriate funtions depending on the search task.
#'
#' @param lookup_keyword The keyword you are looking for
#' @param search_task Defines behavior on search. Defaults to "search" but can also be
#'        "broader" (look for upward definitions) and "narrower" (looking for downward definitions).
#'
#' @return The function returns a vector of keywords
#' @aliases bef.search
#' @import RCurl
#' @import XML
#' @export

bef.tematres.search <- bef.search <- function(lookup_keyword, search_task="search") {
  if (search_task=="search")
    {
      results=bef.tematres.search.keywords(lookup_keyword=lookup_keyword)
      return(results)
    }
  if (search_task=="broader")
    {
      results=bef.tematres.search.broader_keywords(lookup_keyword=lookup_keyword)
      return(results)
    }
  if (search_task=="narrower")
    {
      results=bef.tematres.search.narrower_keywords(lookup_keyword=lookup_keyword)
      return(results)
    }
}
