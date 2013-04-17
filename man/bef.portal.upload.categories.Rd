\name{bef.portal.upload.categories}
\alias{bef.portal.upload.categories}
\alias{bef.upload.categories}
\title{Upload a category set}
\usage{
  bef.portal.upload.categories(datagroup_id, categories,
    curl = getCurlHandle())
}
\arguments{
  \item{datagroup_id}{Categories ID you like to harmonize.
  You can find this in the URL of the category.}

  \item{categories}{Either a data frame with the categories
  or a csv containing the categories and merge ID and stuff
  downloaded from the portal.}

  \item{curl}{Pass in a curl handle with own options or to
  reduce memory footprint.}
}
\value{
  Returns a status message whether the upload was
  successful or not.
}
\description{
  This function is thought for data admin of a BEFdata
  portal. It uses the data haromization features provided
  by the BEFdata portal. In order to do harmonize a
  datagroup you download dthe category with the command
  below: bef.portal.get.categories_for(datagroups_id = 22)
}
\examples{
\dontrun{
  dataframe = my_categories_df
  bef.portal.upload.categories(categories_id = 22, categories = dataframe)
      }
}

