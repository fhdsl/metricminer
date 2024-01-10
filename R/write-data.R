
#' Writes data to a Googlesheet
#' @description This is a function to write metricminer data to a GoogleSheet
#' @param input input data to write to a googlesheet
#' @param gsheet Optionally a googlesheet to write to
#' @param overwrite TRUE/FALSE overwrite if there is data at the destination
#' @return The googlesheet URL where the data has been written
#' @importFrom utils menu installed.packages
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' repo_list <- get_user_repo_list(owner = "metricminer")
#'
#' write_to_gsheet(repo_list)
#' }
#'

write_to_gsheet <- function() {

}



#' Writes data to a tabular file
#' @description This is a function to write metricminer data to a tabular file
#' @param input input data to write to a googlesheet
#' @param file_path A file path where the table should be saved to
#' @param overwrite TRUE/FALSE overwrite if there is data at the destination
#' @param table_type CSV and TSV are options. CSV is default
#' @return The file path where the data has been written
#' @importFrom utils menu installed.packages
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#' repo_list <- get_user_repo_list(owner = "metricminer")
#'
#' write_to_table(repo_list)
#' }
write_to_table <- function() {

}
