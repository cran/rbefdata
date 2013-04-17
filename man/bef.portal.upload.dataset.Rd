\name{bef.portal.upload.dataset}
\alias{bef.portal.upload.dataset}
\alias{bef.upload.dataset}
\title{Upload a dataset to the BEFdata portal}
\usage{
  bef.portal.upload.dataset(dataset, dataset_title,
    curl = getCurlHandle(), open_browser = F)
}
\arguments{
  \item{dataset}{The data you like to upload as dataset.
  This can be a path to a CSV file or a data frame. Please
  not that you have to ensure that the column names are
  unique.}

  \item{dataset_title}{The title of the dataset.}

  \item{curl}{You can pass in a curl handle with additional
  options. By default a curl handle is used to improve the
  memory footprint.}

  \item{open_browser}{If this is set to true the page of
  the dataset is opened in the browser after successful
  upload. This defaults to FALSE.}
}
\value{
  Returns a status message with the ID of the dataset.
}
\description{
  This function uploads a dataset from within R to a
  BEFdata portal set in your options. Before you can do so
  you need to have an account on the portal and set your
  credentials via the bef.options() function so the
  authentication works. The function requires a title for
  the dataset and either a path to a CSV file for for
  upload or a dataset object. The dataset must have unique
  column names.
}
\examples{
\dontrun{
 dataset = data.frame(a = c(1,2,3,4), b = c("a","b","c","d"))
 bef.portal.upload.dataset(dataset = dataset, dataset_title = "Testtitle", open_browser = T)
      }
}

