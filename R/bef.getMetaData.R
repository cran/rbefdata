#' Get the metadata of a dataset from the BEFdata portal
#' 
#' This function fetches metedata of a dataset from the BEFdata portal. 
#' You need to provide the id of the dataset, or the direct link 
#' to its eml file.
#' 
#' @import XML
#' @export
#' 
#' @param dataset_id id of dataset in BEFdata portal. 
#' @param full_url direct link to its eml file. 

bef.getMetaData = function(dataset_id, full_url) {
  if (missing(full_url)) {
     full_url = sprintf("%s/datasets/%d.eml", bef.options('url'), dataset_id)
  }
  eml = xmlParse(full_url)
  lst = list()
  # title
  lst$title = xmlValue(getNodeSet(eml, "//title")[[1]])
  # authors
  lst$authors = data.frame(
    xmlToDataFrame(getNodeSet(eml, "//creator/individualName")), 
    emails = xpathSApply(eml, "//creator/electronicMailAddress", xmlValue))
  # abstract
  lst$abstract = xpathSApply(eml, "//abstract", xmlValue)
  # taxonic extent
  lst$taxonicextent = xpathSApply(eml, "//generalTaxonomicCoverage", xmlValue)
  # geographicCoverage
  lst$spatial$description = xpathSApply(eml, "//geographicDescription", xmlValue)
  # geographicCoordinates
  lst$spatial$corrdinates = xpathSApply(eml, "//boundingCoordinates/*", xmlValue)
  names(lst$spatial$corrdinates) = c('west', 'east', 'north','south')
  # temporal extent
  lst$temporal = xpathSApply(eml, "//rangeOfDates/*/*", function(x) gsub(xmlValue(x), pattern = "\\s", replacement = ""))
  names(lst$temporal) = c("begin", "end")
  # sampling & analyze
  tmp = xpathSApply(eml, "//samplingDescription/para", xmlValue, simplify=F)
  if(length(tmp) != 0) {
    names(tmp) = c("sampling", "analyze")
    lst = c(lst, tmp)
  }
  return(lst)
}
