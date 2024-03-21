#' Get a list of papers that cite your paper
#' @description This is a function to retrieve a list of papers that cite your papers
#' @param paper_cite_link This is not a google citation page. 1. Go to: https://scholar.google.com/scholar 2. Search for the paper we are looking for the citation count. 3. Then click the Cited by ___ button below the title of the paper 4. Copy and paste this url and put it in this get_citation_count() function
#' @export
#' @return A list of the example datasets available in this package
#' @examples \dontrun{
#'
#' paper_cite_link <- "https://scholar.google.com/scholar?cites=6140457238337460780"
#'
#' papers_cited_df <- get_citation_count(paper_cite_link)
#' }
get_citation_count <- function(paper_cite_link) {

  base_url <- "https://scholar.google.com/scholar?cites="

  is_cite_page <- grepl(base_url, paper_cite_link, fixed = TRUE)

  if (!is_cite_page) {
    stop("This is not a google citation page. \n",
         "1. Go to: https://scholar.google.com/scholar \n",
         "2. Search for the paper we are looking for the citation count. \n",
         "3. Then click the Cited by ___ button below the title of the paper \n",
         "4. Copy and paste this url and put it in this get_citation_count() function")
  }

  response <- httr::GET(paper_cite_link)

  if (httr::status_code(response) != 200) {
    httr::stop_for_status(response)
  }

  original_paper <- rvest::read_html(httr::content(response, "text")) %>%
    rvest::html_nodes('h2.gs_rt') %>%
    rvest::html_text()

  cite_titles <- rvest::read_html(httr::content(response, "text")) %>%
    rvest::html_nodes('h3') %>%
    rvest::html_text()

  links <- rvest::read_html(paper_cite_link) %>%
    rvest::html_nodes('h3') %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  df <- data.frame(original_paper, cite_titles, links)
  return(df)
}
