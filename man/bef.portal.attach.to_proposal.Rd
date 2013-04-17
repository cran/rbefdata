\name{bef.portal.attach.to_proposal}
\alias{bef.attach.to_proposal}
\alias{bef.portal.attach.to_proposal}
\title{Attach a freeformat file to a paper proposal}
\usage{
  bef.portal.attach.to_proposal(id, attachment,
    description = "", is_paper = FALSE, doi = "",
    open_browser = FALSE, curl = getCurlHandle())
}
\arguments{
  \item{id}{The ID of the proposal you like to attach to.
  You can get the ID from the URL of the proposal.}

  \item{attachment}{Can be either a path to a file or an R
  data frame.}

  \item{description}{An optional description to your
  attachmed freeformat file.}

  \item{curl}{You can pass in a curl handle to reduce
  memory footprint and to add own options}

  \item{is_paper}{Just to tell the portal if it is a
  published paper you are attaching as pdf for example.
  This parameter can be TRUE or FALSE and defaults to
  FALSE.}

  \item{doi}{If the attachment has a persistent digital
  identifyer you can add it here.}

  \item{open_browser}{If this is set to true the page of
  the proposal is opened in the browser after successful
  upload. This defaults to FALSE.}
}
\value{
  Returns a status message whether the update was
  successful or not.
}
\description{
  This function attaches a freeformat file to a proposal
}
\examples{
\dontrun{
  bef.portal.attach.to_proposal(id = 72, attachment = dataset, description = "This
               is a pure test description for the new attachment file", open_browser = T)
      }
}

