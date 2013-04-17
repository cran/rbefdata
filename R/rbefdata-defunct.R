#' Defunct functions in package rbefdata
#'
#' These following functions are renamed or removed. Please use new functions instead.
#'
#' \tabular{ll}{
#'  \strong{Defunct functions} \tab \strong{Replacement} \cr
#'  \code{bef.getDataset} \tab \code{\link{bef.portal.get.dataset}} \cr
#'  \code{bef.getMetadata} \tab \code{\link{bef.portal.get.metadata}} \cr
#'  \code{bef.getKeywords} \tab \code{\link{bef.portal.get.keywords}} \cr
#'  \code{bef.getProposal} \tab \code{\link{bef.portal.get.proposal}} \cr
#'  \code{bef.searchTematres} \tab \code{\link{bef.tematres.search.keywords}} \cr
#'  \code{bef.searchTematresLower} \tab \code{\link{bef.tematres.search.narrower_keywords}} \cr
#'  \code{bef.searchTematresUpper} \tab \code{\link{bef.tematres.search.broader_keywords}} \cr
#'  \code{bef.portal.get} \tab no replacement \cr
#' }
#'
#' @name rbefdata-defunct
#' @aliases bef.getDataset bef.getKeywords bef.getMetadata bef.getProposal bef.searchTematres bef.searchTematresLower bef.searchTematresUpper bef.portal.get
#' @export bef.getDataset bef.getKeywords bef.getMetadata bef.getProposal bef.searchTematres bef.searchTematresLower bef.searchTematresUpper bef.portal.get
#' @keywords internal
#'

## defunct with version 0.4
#' @rdname rbefdata-defunct
bef.getDataset <- function(...) {
  .Defunct("bef.portal.get.dataset")
}
#' @rdname rbefdata-defunct
bef.getKeywords <- function() {
  .Defunct("bef.portal.get.keywords")
}
#' @rdname rbefdata-defunct
bef.getMetadata <- function(...) {
  .Defunct("bef.portal.get.metadata")
}
#' @rdname rbefdata-defunct
bef.getProposal <- function(...) {
  .Defunct("bef.portal.get.proposal")
}
#' @rdname rbefdata-defunct
bef.searchTematres <- function(...) {
  .Defunct("bef.tematres.search.keywords")
}
#' @rdname rbefdata-defunct
bef.searchTematresLower <- function(...) {
  .Defunct("bef.tematres.search.narrower_keywords")
}
#' @rdname rbefdata-defunct
bef.searchTematresUpper <- function(...) {
  .Defunct("bef.tematres.search.broader_keywords")
}

# version 0.4 defuncts
#' @rdname rbefdata-defunct
bef.portal.get <- function(...) {
  .Defunct("Use the appropriate function for your task instead of this wrapper, e.g:
	    bef.portal.get.attachments(dataset = 72)
	    bef.portal.get.attachments_for(dataset = 72)
	    bef.portal.get.dataset(id = 72)
	    bef.portal.get.dataset_by(id = 72)
	    bef.portal.get.datasets_for_keyword(keyword = 'explanatory')
	    bef.portal.get.keywords()
	    bef.portal.get.metadata(dataset = 72)
	    bef.portal.get.metadata_for(dataset = 72)
	    bef.portal.get.datasets_for_proposal(id = 7) ")
}
