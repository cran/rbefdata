#' Deprecated functions in package rbefdata
#'
#' These following functions are renamed or removed. Please use new functions instead.
#'
#' \tabular{ll}{
#'  \strong{Deprecated functions} \tab \strong{Replacement} \cr
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
#' @name rbefdata-deprecated
#' @aliases bef.getdata bef.getDataset bef.getMetadata bef.getKeywords bef.getProposal
#'          bef.searchTematres bef.searchTematresLower  bef.searchTematresUpper
#' @export bef.getdata bef.getDataset bef.getMetadata bef.getKeywords bef.getProposal bef.searchTematres
#'          bef.searchTematresLower  bef.searchTematresUpper
#' @keywords internal
#'
#' @rdname rbefdata-deprecated
bef.getdata <- function(...) {
  .Deprecated(new = "bef.portal.get")
  bef.portal.get(...)
}
#' @rdname rbefdata-deprecated
bef.getDataset <- function(...) {
  .Deprecated("bef.portal.get.dataset")
  bef.portal.get.dataset(...)
}
#' @rdname rbefdata-deprecated
bef.getKeywords <- function() {
  .Deprecated("bef.portal.get.keywords")
  bef.portal.get.keywords()
}
#' @rdname rbefdata-deprecated
bef.getMetadata <- function(...) {
  .Deprecated("bef.portal.get.metadata")
  bef.portal.get.metadata(...)
}
#' @rdname rbefdata-deprecated
bef.getProposal <- function(...) {
  .Deprecated("bef.portal.get.proposal")
  bef.portal.get.proposal(...)
}
#' @rdname rbefdata-deprecated
bef.searchTematres <- function(...) {
  .Deprecated("bef.tematres.search.keywords")
  bef.tematres.search.keywords(...)
}
#' @rdname rbefdata-deprecated
bef.searchTematresLower <- function(...) {
  .Deprecated("bef.tematres.search.narrower_keywords")
  bef.tematres.search.narrower_keywords(...)
}
#' @rdname rbefdata-deprecated
bef.searchTematresUpper <- function(...) {
  .Deprecated("bef.tematres.search.broader_keywords")
  bef.tematres.search.broader_keywords(...)
}

