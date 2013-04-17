\name{bef.portal.get.attachments}
\alias{bef.get.attachments}
\alias{bef.get.attachments_for}
\alias{bef.portal.get.attachments}
\alias{bef.portal.get.attachments_for}
\title{Download attachment files of a dataset in BEFdata portal}
\usage{
  bef.portal.get.attachments(dataset,
    directory = bef.options("download_dir"),
    curl = getCurlHandle(), ...)
}
\arguments{
  \item{dataset}{The ID of dataset you like to fetch the
  attachments from. You find the ID on the dataset page on
  the BEFdata portal.}

  \item{directory}{The directory to store attachment files
  to. By default it creates a folder called "downloads"
  under the current working directory. The default can be
  changed by bef.options.}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint).}

  \item{\dots}{arguments passed to
  \code{\link[RCurl]{getURLContent}}.}
}
\value{
  A data frame of file information is returned invisibly.
  NULL is returned when the dataset has no attachement
  files.
}
\description{
  This function will download attachment files associated
  with a dataset into specified directory.
}

