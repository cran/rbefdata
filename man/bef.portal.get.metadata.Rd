\name{bef.portal.get.metadata}
\alias{bef.portal.get.metadata}
\title{Get the metadata of a dataset from the BEFdata portal}
\usage{
  bef.portal.get.metadata(dataset_id,
    full_url = dataset_url(dataset_id, "eml"), file)
}
\arguments{
  \item{dataset_id}{The id of the dataset in the BEFdata
  portal.}

  \item{full_url}{functions as direct download link for the
  eml file.}

  \item{file}{path to a local eml file}
}
\value{
  a list of metadata. metadata that doesn't exist is
  represented as \code{NA}
}
\description{
  This function fetches the metadata associated with
  dataset on a BEFdata portal. You need to provide the id
  of the dataset, or the direct link to the Ecological
  Metadata Language file. You can find the url on the
  dataset page.
}

