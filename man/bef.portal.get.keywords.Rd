\name{bef.portal.get.keywords}
\alias{bef.portal.get.keywords}
\title{Fetch keywords from a BEFdata portal.}
\usage{
  bef.portal.get.keywords(curl = getCurlHandle(), ...)
}
\arguments{
  \item{curl}{You can pass in a curl handle with additional
  options. By default a curl handle is used to improve the
  memory footprint.}

  \item{\dots}{Arguments passed to
  \code{\link[RCurl]{getURLContent}}}
}
\value{
  The function returns a data frame of keyword (id, name,
  cound).
}
\description{
  This function fetches keywords from a BEFdata portal.
}
\examples{
\dontrun{
            keywords=bef.portal.get.keywords()
          }
}

