#' Fetch primary data in CSV format from a BEFdata portal dataset.
#'
#' This function fetches data associated with a BEFdata portal dataset. By
#' default it will fetch the CSV file of a dataset. You need to provide the
#' function with a dataset id which you can find in the URL of the dataset on
#' the BEFdata portal and your user credentials. You can find the credentials
#' inside of your profile page on the BEFdata portal. The credentials ensure
#' you have the rights to download the data.
#'
#' The function returns a dataset object which you can store in a variable as
#' shown in the examples below. The object also offers additional information
#' by attributes. You can query the information via the attributes() function
#' which is also shown in the examples. It can also handle multiple dataset at
#' once. In this case you get back a list element of datasets.
#'
#' @param dataset_id This is the id of a dataset on a BEFdata portal. Works also
#'        multiple datasets. In this case you provide the ids with c(1,2,3)
#' @param user_credentials This are your login credentials.
#' @param full_url This functions as direct download link which you can use
#'        instead of the id.
#' @param curl If the function is used inside a loop, call getCurlHandle() first
#'        and pass in the returned value here. This avoids an unnecessary footprint.
#' @param \dots This are other arguments passed to \code{\link[RCurl]{getURLContent}}
#'
#' @return The function returns a dataframe for each dataset. An error is thrown when the dataset is
#'         not found or if you don't have the rights to access it.
#'
#' @examples \dontrun{
#'         datset1 = bef.portal.get.dataset(dataset_id=8, user_credentials="Yy2APsD87JiDbF9YBnU")
#'         attributes(datset1)
#'         dataset2 = bef.portal.get.dataset(
#'           full_url = 'http://befdatadevelepment.biow.uni-leipzig.de/datasets/5/
#'           download.csv?seperate_category_columns=true&user_credentials=Yy2APsD87JiDbF9YBnU')
#'         attributes(dataset2)$author
#'         multi = bef.portal.get.dataset(dataset_id=c(7,8), user_credentials="Yy2APsD87JiDbF9YBnU")
#'       }
#' @aliases bef.get.dataset
#' @import RCurl
#' @export

bef.portal.get.dataset <- bef.get.dataset <- function(dataset_id, user_credentials=bef.options("user_credentials"),
      full_url=dataset_url(dataset_id, user_credentials=user_credentials), curl=getCurlHandle(), ...) {

  if (missing(dataset_id)) dataset_id = url_to_id(full_url)
  df = data.frame(id = dataset_id, full_url = full_url, stringsAsFactors = F)
  df = df[order(df$id), ]

  dataset_list = list()
  for (i in seq_len(nrow(df))) {
    url_content = getURLContent(df$full_url[i], curl = curl)
    if(getCurlInfo(curl)$response.code != 200) {
      msg = sprintf("Dataset(id=%d) not found or not accessible. Please check your credentials and make sure you have access right for it.", df$id[i])
      stop(msg)
    }
    dataset = read.csv(text = url_content)
    metadata = bef.portal.get.metadata(dataset_id=df$id[i])
    attributes(dataset) = c(attributes(dataset), metadata)
    dataset_list[[paste("dataset_id", df$id[i], sep = "_")]] = dataset
  }
  if (length(dataset_list) == 1) return(dataset_list[[1]])
  return(dataset_list)
}
