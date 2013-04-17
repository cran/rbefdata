\name{rbefdata-defunct}
\alias{bef.getDataset}
\alias{bef.getKeywords}
\alias{bef.getMetadata}
\alias{bef.getProposal}
\alias{bef.portal.get}
\alias{bef.searchTematres}
\alias{bef.searchTematresLower}
\alias{bef.searchTematresUpper}
\alias{rbefdata-defunct}
\title{Defunct functions in package rbefdata}
\usage{
  bef.getDataset(...)

  bef.getKeywords()

  bef.getMetadata(...)

  bef.getProposal(...)

  bef.searchTematres(...)

  bef.searchTematresLower(...)

  bef.searchTematresUpper(...)

  bef.portal.get(...)
}
\description{
  These following functions are renamed or removed. Please
  use new functions instead.
}
\details{
  \tabular{ll}{ \strong{Defunct functions} \tab
  \strong{Replacement} \cr \code{bef.getDataset} \tab
  \code{\link{bef.portal.get.dataset}} \cr
  \code{bef.getMetadata} \tab
  \code{\link{bef.portal.get.metadata}} \cr
  \code{bef.getKeywords} \tab
  \code{\link{bef.portal.get.keywords}} \cr
  \code{bef.getProposal} \tab
  \code{\link{bef.portal.get.proposal}} \cr
  \code{bef.searchTematres} \tab
  \code{\link{bef.tematres.search.keywords}} \cr
  \code{bef.searchTematresLower} \tab
  \code{\link{bef.tematres.search.narrower_keywords}} \cr
  \code{bef.searchTematresUpper} \tab
  \code{\link{bef.tematres.search.broader_keywords}} \cr
  \code{bef.portal.get} \tab no replacement \cr }
}
\keyword{internal}

