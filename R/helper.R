# a helper method which behaves like dataset_url in Rails
dataset_url <- function(id, type = c("csv2", "csv", "xls", "eml", "freeformat"), ...) {
  type = match.arg(type, c("csv2", "csv", "xls", "eml", "freeformat"))
  seg = switch(type, csv2="/download.csv", csv="/download.csv", xls="/download", eml=".eml", freeformat="/freeformats_csv" )
  params = Filter(Negate(is.null), list(...))
  if (type == "csv2") params$separate_category_columns = TRUE
  query_string = ""
  if (length(params)) query_string = paste("?", paste(names(params), params, sep = "=", collapse = "&"), sep = "")
  url = sprintf("%s/datasets/%d%s%s", bef.options("url"), id, seg, query_string)
  url = gsub("\\s", "", url)
  return(url)
}

# a helper that returns the keyword url provided the id
keyword_url <- function(id) {
  url = sprintf("%s/keywords/%d", bef.options("url"), id)
  return(url)
}

# a helper method which behaves like paperproposal_url in Rails
paperproposal_url <- function(proposal_id, ...) {
  params = Filter(Negate(is.null), list(...))
  query_string = ""
  if (length(params)) query_string = paste("?", paste(names(params), params, sep = "=", collapse = "&"), sep = "")
  url = sprintf("%s/paperproposals/%d.csv%s", bef.options('url'), proposal_id, query_string)
  url = gsub("\\s", "", url)
  return(url)
}

# a helper function to extract id from a dataset or paperproposal url
url_to_id <- function(url, resource="datasets") {
  pattern = sprintf(".*/%s/(\\d+).*", resource)
  as.numeric(ifelse(grepl(pattern, url), sub(pattern, "\\1", url), NA))
}

# a wrapper function of xpathSApply. which will return NA instead of zero-lenght list when nodes not found
xmlNodesValue <- function(doc, path){
  out = xpathSApply(doc, path, xmlValue, trim=T, ignoreComments=T)
  out = Filter(function(x) x!="", out)
  if (length(out) == 0) return(NA)
  out
}

# for existing file, append a number to its filename
suggest_filename <- function(filename, dir=getwd()) {
  ofn = filename
  i = 0
  while (file.exists(file.path(dir, filename))) {
    i = i + 1
    filename = sub(ofn, pattern="(\\.\\w+)?$", replacement=sprintf("(%d)\\1", i))
  }
  return(filename)
}
