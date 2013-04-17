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

# Helper that determines internet connection
is_internet_connected <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  if(!any(grep(validIP, ipmessage[-grep("127.0.0.1", ipmessage)]))) {
    warning("Sorry not internet connection. Please connect first!")
  }
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
  suggested_filename = filename
  i = 0
  while (file.exists(file.path(dir, suggested_filename))) {
    i = i + 1
    suggested_filename = sub(filename, pattern="(\\.\\w+)?$", replacement=sprintf("(%d)\\1", i))
  }
  return(suggested_filename)
}
