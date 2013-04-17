\name{bef.portal.update.dataset}
\alias{bef.portal.update.dataset}
\alias{bef.update.dataset}
\title{Update a dataset already on BEFdata portal}
\usage{
  bef.portal.update.dataset(dataset, dataset_id,
    warn = TRUE, open_browser = F, curl = getCurlHandle())
}
\arguments{
  \item{dataset}{The data you like to use for upload and to
  update the old dataset. This can be a either a path to a
  CSV file or a data frame.}

  \item{dataset_id}{The ID of the dataset you like to
  update. You can get the ID from the URL of the dataset
  you like to update.}

  \item{warn}{Is a warning that should prevent overriding
  dataset accidentially. Set this to FALSE if you know what
  you do.}

  \item{curl}{You can pass in a curl handle to reduce
  memory footprint and to add own options}

  \item{open_browser}{If this is set to true the page of
  the dataset is opened in the browser after successful
  upload. This defaults to FALSE.}
}
\value{
  Returns a status message whether the update was
  successful or not.
}
\description{
  This function updates a dataset from within R. Before you
  can do so you need to have an account on the portal and
  set your credentials via the bef.options() function so
  the authentication works. The function requires an ID of
  the dataset and either a path to a CSV file or a data
  frame used to update the old dataset.
}
\details{
  Please be careful as this function overrides the old
  dataset (raw data) on the portal completely.
}
\examples{
\dontrun{
 dataset = data.frame(a = c(1,2,3,4), b = c("a","b","c","d"))
 bef.portal.update.dataset(dataset = dataset, dataset_id = 72)
      }
}

