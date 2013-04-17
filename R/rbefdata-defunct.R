#' Defunct functions in package rbefdata
#'
#' These following functions are renamed or removed. Please use new functions instead.
#'
#' \tabular{ll}{
#'  \strong{Defunct functions} \tab \strong{Replacement} \cr
#'  \code{bef.getdata} \tab \code{\link{bef.portal.get}} \cr
#'  \code{bef.getDataset} \tab \code{\link{bef.portal.get.dataset}} \cr
#'  \code{bef.getMetadata} \tab \code{\link{bef.portal.get.metadata}} \cr
#'  \code{bef.getKeywords} \tab \code{\link{bef.portal.get.keywords}} \cr
#'  \code{bef.getProposal} \tab \code{\link{bef.portal.get.proposal}} \cr
#'  \code{bef.searchTematres} \tab \code{\link{bef.tematres.search.keywords}} \cr
#'  \code{bef.searchTematresLower} \tab \code{\link{bef.tematres.search.narrower_keywords}} \cr
#'  \code{bef.searchTematresUpper} \tab \code{\link{bef.tematres.search.broader_keywords}} \cr
#' }
#'
#' @name rbefdata-defunct
#' @aliases rbefdata-defunct bef.getdata bef.getDataset bef.getMetadata bef.getKeywords bef.getProposal
#'          bef.searchTematres bef.searchTematresLower  bef.searchTematresUpper
#' @export bef.getdata bef.getDataset bef.getMetadata bef.getKeywords bef.getProposal bef.searchTematres
#'          bef.searchTematresLower  bef.searchTematresUpper
#' @keywords internal
#'
#' @rdname rbefdata-defunct
bef.getdata <- function(dataset_id, proposal_id, full_url, user_credentials) {
  .Defunct("bef.portal.get")
}

#' @rdname rbefdata-defunct
bef.getDataset <- function(dataset_id, full_url, user_credentials, curl=getCurlHandle(), ...) {
  .Defunct("bef.portal.get.dataset")
}

#' @rdname rbefdata-defunct
bef.getKeywords = function() {
  .Defunct("bef.portal.get.keywords")
}
#' @rdname rbefdata-defunct
bef.getMetadata = function(dataset_id, full_url) {
  .Defunct("bef.portal.get.metadata")
}
#' @rdname rbefdata-defunct
bef.getProposal <- function(proposal_id, user_credentials, full_url, curl=getCurlHandle(), ...) {
  .Defunct("bef.portal.get.proposal")
}
#' @rdname rbefdata-defunct
bef.searchTematres <- function(lookup_keyword) {
  .Defunct("bef.tematres.search.keywords")
}
#' @rdname rbefdata-defunct
bef.searchTematresLower = function(lookup_keyword) {
  .Defunct("bef.tematres.search.narrower_keywords")
}
#' @rdname rbefdata-defunct
bef.searchTematresUpper <- function(lookup_keyword) {
  .Defunct("bef.tematres.search.broader_keywords")
}
