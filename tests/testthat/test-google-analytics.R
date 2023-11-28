
library(testthat)

test_that("Google Analytics Properties", {

  ga_user <- get_ga_user()

  properties_list <- get_ga_properties(account_id = 209776907)

  expect_named(properties_list$properties, c("name", "parent", "createTime",
                                             "updateTime", "displayName", "timeZone", "currencyCode", "serviceLevel",
                                             "account", "propertyType", "industryCategory"))

})


test_that("Google Analytics Metadata", {

  properties_list <- get_ga_properties(account_id = 209776907)

  property_id <- gsub("properties/", "", properties_list$properties$name[1])
  property_metadata <- get_ga_metadata(property_id = property_id)

  expect_named(properties_list$properties, c("dimensions", "metrics", "name"))

})
