#' Get the metadata of a dataset from the BEFdata portal
#'
#' This function fetches the metadata associated with dataset on a BEFdata portal.
#' You need to provide the id of the dataset, or the direct link to the Ecological
#' Metadata Language file. You can find the url on the dataset page.
#'
#' @param dataset_id The id of the dataset in the BEFdata portal.
#' @param full_url functions as direct download link for the eml file.
#' @param file path to a local eml file
#'
#' @return a list of metadata. metadata that doesn't exist is represented as \code{NA}
#' @import XML
#' @export
#'

bef.portal.get.metadata = function(dataset_id, full_url = dataset_url(dataset_id, "eml"), file) {
  is_internet_connected()#?

  if (!missing(file)) full_url = file
  eml = xmlParse(full_url)

  template = list(
    title = "//dataset/title",
    abstract = "//dataset/abstract/para",
    taxonomicextent = "//dataset//generalTaxonomicCoverage",
    sampling = "//dataset//samplingDescription/para",
    spatial_information = list(
      description = "//geographicCoverage/geographicDescription",
      coordinates = c(west = "//geographicCoverage/boundingCoordinates/westBoundingCoordinate",
                    east = "//geographicCoverage/boundingCoordinates/eastBoundingCoordinate",
                    north = "//geographicCoverage/boundingCoordinates/northBoundingCoordinate",
                    south = "//geographicCoverage/boundingCoordinates/southBoundingCoordinate")),
    temporal_information = c(begin = "//temporalCoverage//beginDate", end = "//temporalCoverage//endDate"),
    authors = list(
      firstname = "//creator/individualName/givenName",
      lastname = "//creator/individualName/surName",
      email = "//creator/electronicMailAddress"),
    keywordList = list(
              keywords= "//keywordSet")
  )

  out = rapply(template, function(x) xmlNodesValue(path=x, doc=eml), how="replace")
  out$authors = as.data.frame(out$authors, stringsAsFactors=F)

  attributeList = getNodeSet(eml, path="//attributeList/attribute")

  column_template = list(header = "./attributeLabel",
                         description = "./attributeDefinition",
                         unit = ".//unit",
                         info = ".//attributeName"
                         )

  columns = lapply(column_template, function(c) {
    sapply(attributeList, function(d) {
      xmlNodesValue(doc=d, path=c)
    })
  })

  columns = as.data.frame(columns, stringsAsFactors=F)

  if (nrow(columns)) {
    columns$unit = as.character(columns$unit)
    columns$unit[is.na(columns$unit) | columns$unit == "dimensionless"] = "dimensionless"
    out$columns = columns
  }

  return(out)
}
