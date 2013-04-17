\name{bef.portal.attach.to_dataset}
\alias{bef.attach.to_dataset}
\alias{bef.portal.attach.to_dataset}
\title{Attach a freeformat file to a dataset}
\usage{
  bef.portal.attach.to_dataset(id, attachment,
    description = "", open_browser = FALSE,
    curl = getCurlHandle())
}
\arguments{
  \item{id}{The ID of the dataset you like to attach to.
  You can get the ID from the URL of the dataset.}

  \item{attachment}{Can be either a path to a file or an R
  data frame.}

  \item{description}{An optional description to your
  attachmed freeformat file.}

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
  This function attaches a freeformat file to a dataset.
}
\examples{
\dontrun{
  bef.portal.attach.to_dataset(id = 72, attachment = dataset, description = "This
               is a pure test description for the new attachment file", open_browser = T)
      }
}

