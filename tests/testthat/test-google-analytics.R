library(testthat)

test_that("Google Analytics: Properties", {
  ga_user <- get_ga_user()

  properties_list <- get_ga_properties(account_id = 209776907)

  expect_named(properties_list$properties, c(
    "name", "parent", "createTime",
    "updateTime", "displayName", "timeZone", "currencyCode", "serviceLevel",
    "account", "propertyType", "industryCategory"
  ))
})


test_that("Google Analytics: Stats", {
  properties_list <- get_ga_properties(account_id = 209776907)

  property_id <- gsub("properties/", "", properties_list$properties$name[1])
  property_metadata <- get_ga_metadata(property_id = property_id)

  expect_named(property_metadata, c("dimensions", "metrics", "name"))

  metrics <- get_ga_stats(property_id, stats_type = "metrics")
  expect_named(metrics, c("metricHeaders", "rows", "rowCount", "metadata", "kind"))

  dimensions <- get_ga_stats(property_id, stats_type = "dimensions")
  expect_named(dimensions, c("dimensionHeaders", "rows", "rowCount", "metadata", "kind"))
})

test_that("Google Analytics: All Stats", {
  stats_list <- all_ga_metrics(account_id = 209776907)

  expect_named(stats_list, c("metrics", "dimensions", "link_clicks"))
})
