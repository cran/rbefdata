\name{bef.portal.get.categories_for}
\alias{bef.get.categories_for}
\alias{bef.portal.get.categories_for}
\title{Get a data groups categories as data frame (data admin interface)}
\usage{
  bef.portal.get.categories_for(datagroup_id,
    curl = getCurlHandle(), ...)
}
\arguments{
  \item{datagroup_id}{It the ID of the data group you like
  to get the categories from.}

  \item{curl}{Pass in a curl handle here if you like with
  own options or to reduce the memory footprint.}

  \item{...}{Additional parameters passed to curl.}
}
\value{
  Returns a data frame with the categories ID, short and
  long description and
}
\description{
  This function gets you a data groups categories as data
  frame. You can use the data frame to harmonize your data
  using the last column and the IDs in the first row. After
  you have relinked the data you can upload the data frame
  using the categories upload command.
}
\examples{
\dontrun{
  bef.portal.get.categories_for(datagroup_id = 22)
      }
}

