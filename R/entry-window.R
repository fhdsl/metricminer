
startApp <- function(){
  appDir <- system.file("metricminer-setup-app", package = "metricminer")

  app_results <- list()
  app_results <- shiny::runApp(appDir, display.mode = "normal")

  invisible()
}
