\name{bef.portal.get.keywords}
\alias{bef.get.keywords}
\alias{bef.portal.get.keywords}
\title{Fetch keywords from a BEFdata portal.}
\usage{
  bef.portal.get.keywords()
}
\value{
  The function returns a character vector of keywords.
}
\description{
  This function fetches keywords from a BEFdata portal. It
  also enables the task of fetching all datasets associated
  with one, or a list of keywords.
}
\details{
  (not finished) It delegates the task of fetching multiple
  datasets to bef.portal.get.dataset which returns a data
  frame for one dataset and a list element if there are
  multiple datasets available.

  An error is thrown when dataset is not found or you don't
  have the proper access right for it.
}
\examples{
\dontrun{
            keywords=bef.portal.get.keywords()
          }
}

