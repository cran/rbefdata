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
#' @return The function returns a dataframe. An error is thrown when the dataset is
#'         not found or if you don't have the rights to access it.
#'
#' @examples \dontrun{
#'              datset1 = bef.getDataset(dataset_id=8, user_credentials="Yy2APsD87JiDbF9YBnU")
#'              attributes(datset1) dataset2 = bef.getDataset(full_url = 'http://befdatadevelepment.biow.uni-leipzig.de/datasets/5/download.csv?seperate_category_columns=true&user_credentials=Yy2APsD87JiDbF9YBnU')
#'              attributes(dataset2)$author
#'              multi = bef.getDataset(dataset_id=c(7,8), user_credentials="Yy2APsD87JiDbF9YBnU")
#'           }
#'
#' @import RCurl
#' @export

bef.getDataset <- function(dataset_id, full_url, user_credentials, curl=getCurlHandle(), ...)
  {

    if (!missing(full_url))
      {
        split_full_url = unlist(strsplit(full_url, split="/"))
        get_dataset_index = grep("datasets", unlist(split_full_url))
        metadata = bef.getMetadata(dataset_id=as.numeric(split_full_url[get_dataset_index+1]))
      } else {

        if (length(dataset_id)>1)
          dataset_id=sort(dataset_id)
          {
            dataset_list=list()
            for(i in 1:length(dataset_id))
              {
                if (missing(user_credentials))
                  {
                    full_url = sprintf("%s/datasets/%d/download.csv?seperate_category_columns=true", bef.options('url'), dataset_id[i])
                  } else {
                    full_url = sprintf("%s/datasets/%d/download.csv?seperate_category_columns=true&user_credentials=%s", bef.options('url'), dataset_id[i], user_credentials)
                  }
                dataset = bef.getDataset(full_url=full_url)
                metadata = bef.getMetadata(dataset_id=dataset_id[i])
                dataset = bef.combineObjects(dataset_object=dataset, metadata_object=metadata)
                dataset_list[i] = list(dataset)
                names(dataset_list)[i] = paste("dataset_id", dataset_id[i], sep="_")
              }
            return(dataset_list)
          }

        metadata = bef.getMetadata(dataset_id=dataset_id)
        if (missing(user_credentials))
          {
            full_url = sprintf("%s/datasets/%d/download.csv?seperate_category_columns=true", bef.options('url'), dataset_id)
          } else {
            full_url = sprintf("%s/datasets/%d/download.csv?seperate_category_columns=true&user_credentials=%s", bef.options('url'), dataset_id, user_credentials)
          }
      }

    url_content = getURLContent(full_url, curl = curl, ...)

    if(grepl(url_content, pattern = "^\\s*<html"))
      {
        stop("Dataset not found or not accessible. Please check your credentials and make sure you have access right for it.")
      }

    connection = textConnection(url_content)
    on.exit(close(connection))
    dataset = read.csv(connection)

    dataset = bef.combineObjects(dataset_object=dataset, metadata_object=metadata)

    return(dataset)
  }
