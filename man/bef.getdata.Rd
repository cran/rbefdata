\name{bef.getdata}
\alias{bef.getdata}
\title{This is a wrapper for the getter functions of the BEFdata R package.}
\usage{
  bef.getdata(dataset_id, proposal_id, full_url,
    user_credentials)
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
}
\value{
  The function returns a dataframe in case of a single
  dataset and a list of objects in case of a paper proposal
  as well as for multiple datasets defined via ids. An
  error is thrown when the dataset or proposal is not found
  or you don't have the proper access right to perform the
  action. For detailed informations about the used
  functions see bef.getData() and bef.getProposal().
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
 dataset = bef.getdata(dataset_id=8, user_credentials="Yy2APsD87JiDbF9YBnU")
 proposal = bef.getdata(proposal_id=7, user_credentials="Yy2APsD87JiDbF9YBnU")
 dataset_full_url = bef.getdata(full_url = 'http://befdatadevelepment.biow.uni-leipzig.de/datasets/5/download.csv?seperate_category_columns=true&user_credentials=Yy2APsD87JiDbF9YBnU')
 multiple = bef.getdata(dataset_id=c(7,8), user_credentials="Yy2APsD87JiDbF9YBnU")
 }
}

