#' Upload a dataset to the BEFdata portal
#'
#' This function uploads a dataset from within R to a BEFdata portal set in
#' your options. Before you can do so you need to have an account on the portal
#' and set your credentials via the bef.options() function so the authentication works.
#' The function requires a title for the dataset and either a path to a CSV file for
#' for upload or a dataset object. The dataset must have unique column names.
#'
#' @param dataset The data you like to upload as dataset. This can be a path to a CSV file
#'        or a data frame. Please not that you have to ensure that the column names are unique.
#' @param dataset_title The title of the dataset.
#' @param curl You can pass in a curl handle with additional options. By default a curl handle is
#'        used to improve the memory footprint.
#' @param open_browser If this is set to true the page of the dataset is opened in the
#'        broser after successful upload. This defaults to FALSE.
#' @return Returns a status message with the ID of the dataset.
#' @import RCurl
#' @import XML
#' @export bef.portal.upload.dataset

bef.portal.upload.dataset <- function(dataset, dataset_title, curl = getCurlHandle(), open_browser = F) {
  this_function_requires_api_authentication()
  if(the_title_is_taken(dataset_title = dataset_title)) {
    stop("The title you have choosen has already been taken. Please choose another one before uploading again!")
  } else {
    postForm(upload_url(), title = dataset_title,  "datafile[file]" = upload_file(dataset), curl = curl)
    if(getCurlInfo(curl)$response.code != 200) {
      stop("Your upload failed. Try again later!")
    } else {
      id = title_to_dataset_id(dataset_title)
      message("Your data has been uploaded successfully!")
      message(paste0("You can find your dataset now under the id:", id))
      if(open_browser) {
	bef.goto.dataset_page(id = id)
      }
    }
  }
}
