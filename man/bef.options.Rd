\name{bef.options}
\alias{bef.options}
\title{Set or query BEFdata portal URL address}
\usage{
  bef.options(...)
}
\arguments{
  \item{\dots}{similar to \code{\link{options}}. see
  examples below.}
}
\description{
  This function is used to query and set the BEFdata portal
  URL address. It is not supposed to be changed, unless you
  are using Beijing portal or other portals using the code
  from the BEFdata project.
}
\examples{
# query BEFdata portal's URL
bef.options('url')
# set URL
bef.options(url='http://www.example.com')
}

