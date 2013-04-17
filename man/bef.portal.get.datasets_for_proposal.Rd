\name{bef.portal.get.datasets_for_proposal}
\alias{bef.portal.get.datasets_for_proposal}
\title{Fetch primary data in CSV format from a BEFdata portal paperproposal}
\usage{
  bef.portal.get.datasets_for_proposal(id,
    curl = getCurlHandle(), ...)
}
\arguments{
  \item{id}{This is the ID of a paper proposal. You can
  download all datasets in one turn given the ID.}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}

  \item{\dots}{This are other arguments passed to
  \code{\link[RCurl]{getURLContent}}}
}
\value{
  The function returns a data frame with information about
  the proposals attached datasets. An error is thrown when
  the proposal is not found or you don't have the access
  rights for it.
}
\description{
  This function fetches data associated to a paperproposal
  on a BEFdata portal.  By default it will fetch all
  associated files in CSV format next to the informations
  available like the title, the download URL and the id of
  the datasets.
}
\details{
  You need to provide the function with a proposal id which
  you can find in the URL of the proposal and your user
  credentials. You can find the credentials inside of your
  profile page on the BEFdata portal. The credentials
  ensure you have the rights to download the data.

  The function returns a list object which you can store to
  a variable as shown in the examples below.
}
\examples{
\dontrun{
	  prop1 = bef.portal.get.datasets_for_proposal(proposal = 8)
 	}
}

