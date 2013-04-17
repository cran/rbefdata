#' Fetch primary data in CSV format from a BEFdata portal paperproposal
#'
#' This function fetches data associated to a paperproposal on a BEFdata
#' portal.  By default it will fetch all associated files in CSV format next to
#' the informations available like the title, the download URL and the id of
#' the datasets.
#'
#' You need to provide the function with a proposal id which you can find in
#' the URL of the proposal and your user credentials. You can find the credentials
#' inside of your profile page on the BEFdata portal. The credentials ensure you
#' have the rights to download the data.
#'
#' The function returns a list object which you can store to a variable as
#' shown in the examples below.
#'
#' @param proposal_id This is the id of a paper proposal. You can download all datasets in
#'        one turn given the proposal_id and your user credentials.
#' @param user_credentials This are your login credentials
#' @param full_url This functions as a download link and you can use is instead of the
#'        proposal_id.
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'        the returned value in here (avoids unnecessary footprint)
#' @param \dots This are other arguments passed to \code{\link[RCurl]{getURLContent}}
#'
#' @return The function returns a list. An error is thrown when the proposal is not found
#'         or you don't have the access rights for it.
#'
#' @examples \dontrun{
#'  prop1 = bef.portal.get.proposal(proposal_id=8, user_credentials="Yy2APsD87JiDbF9YBnU")
#'  prop1
#'  }
#' @aliases bef.get.proposal
#' @import RCurl
#' @export

bef.portal.get.proposal <- bef.get.proposal <- function(proposal_id, user_credentials=bef.options('user_credentials'), full_url=paperproposal_url(proposal_id, user_credentials=user_credentials), curl=getCurlHandle(), ...) {

  # The following chunk generates paperproposal csv URL from paperproposal's URL
  if (!missing(full_url) && !grepl(full_url, pattern="*.csv*")) {
    full_url = paperproposal_url(url_to_id(full_url, "paperproposals"), user_credentials=user_credentials)
  }


  proposal_raw_csv = getURLContent(full_url, curl = curl, ...)
  if (getCurlInfo(curl)$response.code != 200) {
    stop("Proposal not found or not accessible. Please check your credentials and make sure you have access right for it.")
  }

  proposal_csv = read.csv(text = proposal_raw_csv)

  lst = list()
  for (i in 1:nrow(proposal_csv)) {
    lst$dataset_id[i] = toString(proposal_csv[i, "ID"])
    lst$dataset_title[i] = toString(proposal_csv[i,"Title"])
    lst$dataset_url[i] = toString(proposal_csv[i,"Dataset.Url"])
    lst$dataset_csv_url[i] = toString(proposal_csv[i,"CSV.download"])
    lst$dataset_csv[i] = list(bef.portal.get.dataset(full_url=toString(proposal_csv[i,"CSV.download"])))
  }
  return(lst)
}
