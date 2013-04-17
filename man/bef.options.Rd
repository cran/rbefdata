\name{bef.options}
\alias{bef.options}
\title{Set or query otions related to the BEFdata R package.}
\usage{
  bef.options(...)
}
\arguments{
  \item{\dots}{similar to \code{\link{options}}. see
  examples below.}
}
\description{
  This function is used to query and set R BEFdata package
  specific options like the URL to the BEFdata portal and
  the URL to the tematres thesaurus.
}
\examples{
# BEFdata URL
bef.options('url')
bef.options(url='http://www.example.com')
#Tematres URL
bef.options('temtres_url')
bef.options(tematres_url="http://www.example.com")
}

