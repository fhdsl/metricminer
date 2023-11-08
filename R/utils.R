

#' Get file path to an default credentials RDS
default_creds_path <- function(endpoint) {
  list.files(
    pattern = paste0(endpoint, "_default_creds.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function(endpoint) {
  list.files(
    pattern = paste0(endpoint, "_encrypt_pass.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
#' Get file path to an encrypted credentials RDS
encrypt_creds_path <- function() {
  list.files(
    pattern = paste0(endpoint, "_encrypt.rds"),
    recursive = TRUE,
    system.file("extdata", package = "metricminer"),
    full.names = TRUE
  )
}
