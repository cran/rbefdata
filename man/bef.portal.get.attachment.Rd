\name{bef.portal.get.attachment}
\alias{bef.portal.get.attachment}
\title{Download attachment files of a dataset in BEFdata portal}
\usage{
  bef.portal.get.attachment(dataset_id,
    user_credentials = bef.options("user_credentials"),
    dir = bef.options("download_dir"),
    curl = getCurlHandle(), ...)
}
\arguments{
  \item{dataset_id}{The id of dataset}

  \item{user_credentials}{Your login credentials}

  \item{dir}{The directory to store attachment files.
  default to the current working directory.}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint).}

  \item{\dots}{arguments passed to
  \code{\link[RCurl]{getURLContent}}.}
}
\value{
  a data frame of file information is returned invisibly.
  NULL is returned when the dataset has no attachement
  files.
}
\description{
  This function will download attachment files associated
  with a dataset into specified directory.
}

