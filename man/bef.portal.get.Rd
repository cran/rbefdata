\name{bef.portal.get}
\alias{bef.portal.get}
\title{This is a wrapper for the getter functions of the BEFdata R package.}
\usage{
  bef.portal.get(dataset_id, proposal_id, full_url, file,
    user_credentials = bef.options("user_credentials"),
    keyword, keyword_id, keywords = FALSE,
    metadata = FALSE, attachments = FALSE,
    dir = bef.options("download_dir"))
}
\arguments{
  \item{dataset_id}{This is the id of a dataset on a
  BEFdata portal. You find the id in the url of the dataset
  or on the download page of the dataset.}

  \item{proposal_id}{This is the id of a paperproposal}

  \item{user_credentials}{This are the login credentials
  required to verify the user identity. They ensure, you
  have the rights to download the data.}

  \item{full_url}{This option works as direct download
  link. It can be used instead of the according ids. You
  can find the URL either on the dataset download page for
  a dataset or on the proposal page for a paper proposal.}

  \item{keywords}{Can be true or false. If it is defined as
  true the wrapper will get all keywords associated with
  the BEFdata portal.}

  \item{keyword}{Is a keyword that you like to get the
  associated datasets for.}

  \item{keyword_id}{Is the id of a keyword you like to get
  the associated datastes for.}

  \item{metadata}{Can be true or false. If true it will get
  the metadata of a dataset.}

  \item{attachments}{This is true or false and will fetch
  the attached freeformat files associated with a dataset.}

  \item{dir}{This is the download directory for
  attachments. This can be set via bef.options and the
  default download directory is called "download"}

  \item{file}{Hand over a file path. Functions that can
  read a file like the metadata function make use of it.}
}
\value{
  The function returns a dataframe in case of a single
  dataset and a list of objects in case of a paper proposal
  as well as for multiple datasets defined via ids. An
  error is thrown when the dataset or proposal is not found
  or you don't have the proper access right to perform the
  action. For detailed informations about the used
  functions see \code{\link{bef.portal.get.dataset}} and
  \code{\link{bef.portal.get.proposal}}.
}
\description{
  It returns contents depending on the on the parameters
  you provide to the function. It will fetch a dataset if
  you provide the id. If you provide a proposal id it will
  fetch all datasets associated with that proposal. You can
  even provide the function with a full URL of either of
  them and it will fetch the right contents by parsing the
  URL.
}
\examples{
\dontrun{
 dataset = bef.portal.get(dataset_id=8, user_credentials="Yy2APsD87JiDbF9YBnU")
 proposal = bef.portal.get(proposal_id=7, user_credentials="Yy2APsD87JiDbF9YBnU")
 dataset_full_url = bef.portal.get(full_url = 'http://befdatadevelepment.biow.uni-leipzig.de/
   datasets/5/download.csv?seperate_category_columns=true&user_credentials=Yy2APsD87JiDbF9YBnU')
 multiple = bef.portal.get(dataset_id=c(7,8), user_credentials="Yy2APsD87JiDbF9YBnU")
 }
}

