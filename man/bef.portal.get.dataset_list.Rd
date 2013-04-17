\name{bef.portal.get.dataset_list}
\alias{bef.get.dataset_list}
\alias{bef.portal.get.dataset_list}
\title{Fetch a list of datasets for a keyword}
\usage{
  bef.portal.get.dataset_list(keyword, keyword_id)
}
\arguments{
  \item{keyword}{The keyword you like to fetch the
  associated datasets for}

  \item{keyword_id}{This is the id of the keyword}
}
\description{
  This function fetches a list of datasets associated with
  a BEFdata portal keyword.
}
\examples{
\dontrun{
        list = bef.portal.get.dataset_list(keyword="carbon")
        list = bef.portal.get.dataset_list(keyword_id=47)
      }
}

