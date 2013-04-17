#' Download attachment files of a dataset in BEFdata portal
#'
#' This function will download attachment files associated with a dataset into specified directory.
#'
#' @return a data frame of file information is returned invisibly. NULL is returned when
#'         the dataset has no attachement files.
#'
#' @param dataset_id The id of dataset
#' @param user_credentials Your login credentials
#' @param dir The directory to store attachment files. default to the current working directory.
#' @param curl If using in a loop, call getCurlHandle() first and pass the returned value
#'         in here (avoids unnecessary footprint).
#' @param \dots  arguments passed to \code{\link[RCurl]{getURLContent}}.
#' @export

bef.portal.get.attachment <- function(dataset_id, user_credentials=bef.options('user_credentials'), dir=bef.options('download_dir'), curl=getCurlHandle(), ...) {
  url = dataset_url(dataset_id, "freeformat", user_credentials=user_credentials)
  freeformats_csv = getURLContent(url, curl=curl, ...)
  if (getCurlInfo(curl)$response.code != 200) {
    stop("Dataset not found or not accessible. Please check your credentials and make sure you have access to it")
  }
  files = read.csv(text = freeformats_csv, stringsAsFactors=F)

  if (nrow(files)) {
    if (!file.exists(dir)) dir.create(dir)
    files$path = file.path(dir, sapply(files$Filename, suggest_filename, dir=dir, USE.NAMES=T))
    for (i in seq_len(nrow(files))) {
      f = CFILE(files$path[i], mode="wb")
      cat(sprintf("Saving %s => %s\n", sQuote(files$Filename[i]), sQuote(files$path[i])))
      flush.console()
      curlPerform(url = files$URL[i], writedata = f@ref)
      close(f)
    }
    invisible(files)
  } else {
    warning(paste("No attachement files for dataset:", dataset_id))
    invisible(NULL)
  }
}
