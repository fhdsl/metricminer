# CRAN

 library("ggplot2")
 library("dlstats")

 download_stats <- cran_stats(c("ottrpal", "metricminer", "conrad", "ari", "text2speech", "mario"))

 download_stats %>% dplyr::summarize(download_total = sum(downloads))

 download_stats %>% dplyr::group_by(package) %>%
   dplyr::summarize(download_total = sum(downloads))



if (!is.null(download_stats)) {
  print(head(download_stats))
  ggplot(download_stats, aes(end, downloads, group=package, color=package)) +
    geom_line() +
    geom_point() +
    scale_y_log10()
 }
