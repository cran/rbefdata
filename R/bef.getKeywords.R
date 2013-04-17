#' Fetch keywords from a BEFdata portal.
#'
#' This function fetches keywords from a BEFdata portal. It also enables the
#' task of fetching all datasets associated with one, or a list of keywords.
#'
#' (not finished) It delegates the task of fetching multiple datasets to
#' bef.getDataset which returns a data frame for one dataset and a list element
#' if there are multiple datasets available.
#'
#' An error is thrown when dataset is not found or you don't have
#' the proper access right for it.
#'
#' @return The function returns a character vector of keywords.
#'
#' @examples \dontrun{
#'             keywords=bef.getKeywords()
#'           }
#'
#' @import RCurl
#' @import rjson
#' @import XML
#' @export

bef.getKeywords = function()
   {
      keywords_json=fromJSON(getURL(paste0(bef.options('url'),"/keywords.json")))
      keywords_summary=unlist(lapply(keywords_json, function(x) (x$name)))
      return(keywords_summary)
   }
