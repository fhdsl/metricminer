auth_tokens <-
  c(
    Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
    Sys.getenv("METRICMINER_GOOGLE_ACCESS")
  )

if (all(!(auth_tokens == ""))) {
  test_that("Google Analytics: Properties", {
    # Interactively create google token
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = FALSE,
      in_test = TRUE
    )

    ga_user <- get_ga_user()
    properties_list <- get_ga_properties(account_id = 209776907)

    expect_named(properties_list, c(
      "name", "parent", "createTime",
      "updateTime", "displayName", "timeZone", "currencyCode", "serviceLevel",
      "account", "propertyType", "industryCategory"
    ))
  })


  test_that("Google Analytics: Stats", {
    # Interactively create google token
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = FALSE,
      in_test = TRUE
    )

    properties_list <- get_ga_properties(account_id = 209776907)

    property_id <- gsub("properties/", "", properties_list$name[1])
    property_metadata <- get_ga_metadata(property_id = property_id)

    expect_named(property_metadata, c("dimensions", "metrics", "name", "comparisons"))

    property_info <- get_ga_property_info(property_id)
    expect_length(property_info, 10)

    metrics <- get_ga_stats(property_id, stats_type = "metrics")
    expect_named(metrics, c(
      "website", "activeUsers", "newUsers", "totalUsers",
      "eventCountPerUser", "screenPageViewsPerUser", "sessions",
      "averageSessionDuration", "screenPageViews", "engagementRate"
    ))

    pages <- get_ga_stats(property_id, stats_type = "pages")
    
    expect_type(pages, "data.frame")
    
    expect_named(pages, c(
      "website_parent", "page", "activeUsers", "newUsers", "totalUsers",
      "eventCountPerUser", "screenPageViewsPerUser", "sessions",
      "averageSessionDuration", "screenPageViews", "engagementRate"
    ))
    
    dimensions <- get_ga_stats(property_id, stats_type = "dimensions")
    expect_named(dimensions, c("day", "month", "year", "country", "fullPageUrl"))
  })

  test_that("Google Analytics: All Stats", {
    # Interactively create google token
    auth_from_secret("google",
      refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
      access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
      cache = FALSE,
      in_test = TRUE
    )

    stats_list <- get_multiple_ga_metrics(account_id = 209776907)

    expect_named(stats_list, c("metrics", "dimensions", "link_clicks"))

    stats_list <- get_multiple_ga_metrics(property_ids = c(422671031, 422558989))

    expect_named(stats_list, c("metrics", "dimensions", "link_clicks"))
    
    expect_type(stats_list$metrics, "data.frame") #make sure there's dimension for the clean dataframe
    
    expect_type(stats_list$dimensions, "data.frame")
    
    expect_type(stats_list$link_clicks, "data.frame")
    
    stats_list <- get_multiple_ga_metrics(property_ids = c(422671031, 422558989), 
                                          stats_type = c("metrics", "dimensions", "link_clicks", "pages"))
    
    expect_named(stats_list, c("metrics", "dimensions", "link_clicks", "pages"))
    
    expect_type(stats_list$pages, "data.frame") #make sure there's dimension for the clean dataframe
    
    stats_list <- get_multiple_ga_metrics(property_ids = c(422671031, 422558989),
                                          stats_type = c("metrics", "dimensions"))
    
    expect_named(stats_list, c("metrics", "dimensions"))
    
    
  })
} else {
  message("testthat tests skipped because no auth detected")
}
