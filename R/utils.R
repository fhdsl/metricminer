#' Get file path to an default credentials RDS
default_creds_path <- function(app_name) {
  list.files(
    pattern = paste0(app_name, "_default_creds.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
#' Get file path to an encrypted credentials RDS
encrypt_creds_path <- function(app_name) {
  list.files(
    pattern = paste0(app_name, "_encrypt.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
