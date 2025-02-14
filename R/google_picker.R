launch_picker <- function() {
  token <- get_token("google")

  script <- source(file.path(system.file("extdata", package = "metricminer"), "file_picker.R"))

  # Launch browser and capture output
  process <- processx::process$new(
    "R",
    c("-e",
      sprintf('browseURL("%s")',
              temp_html)),
    stdout = "pipe"
  )

  # Get the output
  raw_output <- process$wait()
  if (!process$get_exit_status()) {
    result <- jsonlite::fromJSON(raw_output)
    print(result)
  } else {
    stop("Failed to get picker output")
  }
}
