


#' Google Scholar Citation number
#' @description This is a function to get how many times a paper has been cited on Google Scholar
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query_params The body parameters for the request
#' @param return_request Should a list of the request be returned as well?
#' @returns This function returns a list from a API response JSON file
#' @importFrom httr GET add_headers
#' @export
#'
#'
doi <- "10.1080/26939169.2022.2118646"

headers = c("User-Agent" = "Safari/537.36")
r <- httr::GET(url = "https://scholar.google.com/scholar?cites=6140457238337460780
               &as_sdt=20000005&sciodt=0,21&hl=en",
               httr::add_headers(.headers=headers))

r |> content() |> html_element('form[method=post] + div div > div:contains("results")') |> html_text()
