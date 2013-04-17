\name{rbefdata-deprecated}
\alias{bef.portal.get.dataset_list}
\alias{bef.portal.get.proposal}
\alias{bef.tematres.search.broader_keywords}
\alias{bef.tematres.search.keywords}
\alias{bef.tematres.search.narrower_keywords}
\alias{rbefdata-deprecated}
\title{Deprecated functions in package rbefdata}
\usage{
  bef.tematres.search.keywords(...)

  bef.tematres.search.narrower_keywords(...)

  bef.tematres.search.broader_keywords(...)

  bef.portal.get.dataset_list(...)

  bef.portal.get.proposal(...)
}
\description{
  These following functions are renamed or removed. Please
  use new functions instead.
}
\details{
  \tabular{ll}{ \strong{Deprecated functions} \tab
  \strong{Replacement} \cr
  \code{bef.tematres.search.keywords} \tab
  \code{\link{bef.tematres.api.search}} \cr
  \code{bef.tematres.search.narrower_keywords} \tab
  \code{\link{bef.tematres.api.search}} \cr
  \code{bef.tematres.search.broader_keywords} \tab
  \code{\link{bef.tematres.api.search}} \cr
  \code{bef.portal.get.dataset_list} \tab
  \code{\link{bef.portal.get.datasets_for_keyword}} \cr
  \code{bef.portal.get.proposal} \tab
  \code{\link{bef.portal.get.datasets_for_proposal}} \cr }
}
\keyword{internal}

