\name{bef.portal.get.metadata}
\alias{bef.get.metadata}
\alias{bef.get.metadata_for}
\alias{bef.portal.get.metadata}
\alias{bef.portal.get.metadata_for}
\title{Get the metadata of a dataset from the BEFdata portal}
\usage{
  bef.portal.get.metadata(dataset)
}
\arguments{
  \item{dataset}{This is either a dataset ID or a file path
  to a eml metadata file.}
}
\value{
  A list of metadata. Metadata That doesn't exist is
  represented as \code{NA}
}
\description{
  This function fetches the metadata associated with a
  dataset on a BEFdata portal. You need to provide the
  function with an id of the dataset or a path to an eml
  file you downloaded from the portal manually. You can
  find the ID in the URL of the dataset page in the portal
  (e.g ID 6: .../datasets/6).
}

