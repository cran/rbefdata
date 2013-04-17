# This can hold helpers maybe also some sort of factories to create a certain
# condition for testing setting the right options. This helper needs to be
# sourced into tests that make use of it with:

# source("test.helpers.R")

given_the_user_is <- function(condition = NULL) {
  if(condition == "valid") {
    valid_credentials = "Yy2APsD87JiDbF9YBnU"
    bef.options(user_credentials = valid_credentials)
    return(valid_credentials)
  }
  if(condition == "invalid") {
    bef.options(user_credentials="invalid")
    return("invalid")
  } else {
    warning("Valid parameters are: valid and invalid")
  }
}

given_the_portal_is <- function(environment = NULL) {
  if(environment == "development") {
    bef.options(url = "http://befdatadevelepment.biow.uni-leipzig.de")
    return("development")
  }
  if(environment == "production") {
    bef.options(url = "http://befdataproduction.biow.uni-leipzig.de")
    return("production")
  } else {
    warning("Valid parameters are: development and production")
  }
}
