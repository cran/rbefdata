\name{bef.portal.api.dataset_info}
\alias{bef.portal.api.dataset_info}
\title{Basic access to the BEFdata portal XML API.}
\usage{
  bef.portal.api.dataset_info(id)
}
\arguments{
  \item{id}{Is the ID of the dataset you like to get the
  information for. You find the ID in the URL of the
  dataset on the BEFdata portal.}
}
\value{
  Returns a list of metadata information for a dataset.
}
\description{
  This function gives access to the meta data available for
  a dataset
}
\details{
  This function takes a dataset id and returns the metadata
  of the dataset as list. This is not the eml metadata. It
  is the metadata read from the XML API of the data portal.
}

