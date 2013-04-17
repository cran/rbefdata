\name{bef.portal.get.dataset}
\alias{bef.get.dataset}
\alias{bef.get.dataset_by}
\alias{bef.portal.get.dataset}
\alias{bef.portal.get.dataset_by}
\title{Fetch primary data from a BEFdata portal dataset.}
\usage{
  bef.portal.get.dataset(id, curl = getCurlHandle(), ...)
}
\arguments{
  \item{id}{This is the ID of a dataset on a BEFdata
  portal.}

  \item{curl}{If the function is used inside a loop, call
  getCurlHandle() first and pass in the returned value
  here. This avoids an unnecessary footprint.}

  \item{\dots}{Arguments passed to
  \code{\link[RCurl]{getURLContent}}}
}
\value{
  The function returns a data frame of the dataset. An
  error is thrown when the dataset is not found or if you
  don't have the rights to access it.
}
\description{
  This function fetches data associated with a BEFdata
  portal dataset. By default it will fetch the CSV file of
  a dataset. You need to provide the function with a
  dataset id which you can find in the URL of the dataset
  on the BEFdata portal. As this usually requires
  authentication you need to set your credentials in the
  options (bef.options("user_credentials" = "asdfpoj"). You
  can find the credentials inside of your profile page on
  the BEFdata portal. The credentials ensure you have the
  rights to download the data.
}
\details{
  The function returns a dataset object which you can store
  in a variable as shown in the examples below. The object
  also offers additional information by attributes. You can
  query the information via the attributes() function which
  is also shown in the examples. If you like to fetch
  multiple datasets you can use the apply functions
  provided by R see example below.
}
\examples{
\dontrun{
        datset1 = bef.portal.get.dataset(id=8)
        metadata1 = attributes(dataset1)

        curl = getCurlHandle()
        ids = c(8,70)
        dataset_list = lapply(ids, function(x) bef.portal.get.dataset_by(id = x, curl = curl))
        metadata = attributes(dataset_list[[1]])
      }
}

