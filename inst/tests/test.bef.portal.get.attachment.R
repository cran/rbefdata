source("test.helpers.R")

context("Get files associated to a dataset with bef.portal.get.attachment()")

test_that("it throws an error if there is no associated files", {
  given_the_portal_is(environment = "development")
  given_the_user_is(condition = "valid")
  expect_warning(bef.portal.get.attachment(dataset_id=8), "*No attachement files for dataset:*")
})

test_that("it creates a new folder for the download files", {
  given_the_portal_is(environment = "development")
  given_the_user_is(condition = "valid")
  folder = bef.options("download_dir")
  bef.portal.get.attachment(dataset_id=7)
  expect_that(file.exists(folder), is_true())
  if (file.exists(folder)) {
    unlink(folder, recursive = TRUE)
  }
})
