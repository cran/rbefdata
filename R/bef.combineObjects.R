#' This is a helper file that contains functions used by the BEFdata R package
#' Internally.
#'
#' @param dataset_object This a BEFdata dataset
#' @param dataset_url This is the url to a BEFdata dataset
#' @param metadata_object This is a metadata object you can get via bef.getMetadata
#' @param metadata_url This is the URL to a BEFdata dataset eml file
#'
#' @import RCurl
#' @import XML
#' @export

bef.combineObjects <- function(dataset_object, dataset_url, metadata_object, metadata_url)
  {
    if(!missing(metadata_url))
      {
        metadata = bef.getMetadata(full_url=metadata_url)
        dataset = dataset_object
      }

    if(!missing(dataset_url))
      {
        dataset = bef.getDataset(full_url=dataset_url)
        metadata = metadata_object
      }

    if(!missing(dataset_object) && !missing(metadata_object))
      {
        dataset = dataset_object
        metadata = metadata_object
      }

    attr(dataset, "title") = metadata$title
    attr(dataset, "authors") = metadata$authors
    attr(dataset, "abstract") = metadata$abstract
    attr(dataset, "taxonicextent") = metadata$taxonicextent
    attr(dataset, "spatial_information") = metadata$spatial
    attr(dataset, "temporal_information") = metadata$temporal
    attr(dataset, "sampling") = metadata$temporal
    attr(dataset, "analyze") = metadata$analyze

    return(dataset)
  }
