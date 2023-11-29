# Extracting data from Google Forms

#' Get Google Forms
#' @description This is a function to get the Calendly API user info
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query A list to be passed to query
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
request_google_forms <- function(token, url, query = NULL, body_params = NULL) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  result <- httr::GET(
      url = url,
      body = body_params,
      query = query,
      config = config,
      httr::accept_json(),
      encode = "json"
    )

  if (httr::status_code(result) != 200) {
    return(httr::content(result, "text"))
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)
  return(result_list)
}


#' Get Google Forms
#' @description This is a function to get the Calendly API user info
#' @param form_id The form ID we need to get
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @examples \dontrun{
#'
#' authorize("google")
#' form_info <- get_google_form("https://docs.google.com/forms/d/1Z-lMMdUyubUqIvaSXeDu1tlB7_QpNTzOk3kfzjP2Uuo/edit")
#'
#' ### OR You can give it a direct form id
#' form_info <- get_google_form("1Z-lMMdUyubUqIvaSXeDu1tlB7_QpNTzOk3kfzjP2Uuo")
#'
#' }
get_google_form <- function(form_id, token = NULL) {

  # If a URL is supplied, only take the ID from it.
  if (grepl("https:", form_id)) {
    form_id <- gsub("\\/viewform$|\\/edit$", "", form_id)
    form_id <- gsub("https://docs.google.com/forms/d/e/|https://docs.google.com/forms/d/", "", form_id)
  }

  form_info_url <- gsub("\\{formId\\}", form_id, "https://forms.googleapis.com/v1/forms/{formId}")
  form_response_url <- gsub("\\{formId\\}", form_id, "https://forms.googleapis.com/v1/forms/{formId}/responses")

  form_info <- request_google_forms(
    url = form_info_url,
    token = token
  )

  response_info <- request_google_forms(
    url = form_response_url,
    token = token
  )

  result <- list(form_metadata = form_info,
                 response_info = response_info)

  return(result)
}


#' Get multiple Google forms
#' @description This is a function to get the Calendly API user info
#' @param form_ids a vector of form ids you'd like to retrieve information for
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @examples \dontrun{
#'
#' authorize("google")

#' googledrive::drive_auth(token = get_token("google"))
#' form_list <- googledrive::drive_find(shared_drive = googledrive::as_id("0AJb5Zemj0AAkUk9PVA"), type = "form")
#'
#' multiple_forms <- get_folder_of_forms(form_ids = form_list$id)
#'
#' }

get_folder_of_forms <- function(form_ids = NULL, folder_id = NULL, token = NULL) {

  if (is.null(folder_id) && is.null(form_ids)) {
    stop("Neither a vector of form_ids nor a Google Drive folder Id where to look for forms was supplied. Stopping.")
  }

  if (is.null(folder_id)) {
    form_ids <- get_google_files(
      drive_id = folder_id,
      token = token,
      type = "forms")

    form_ids <- form_ids$id

    if (length(form_ids) == 0) stop("No forms were found in the folder provided")
  }

  all_form_info <- lapply(form_ids, function(form_id) {
    form_info <- get_google_form(
      form_id = form_id,
      token = token)

    return(list(form_metadata = form_info, form_name = form_info$form_info$info$documentTitle))
  })

  form_names <- unlist(purrr::map(all_form_info, "form_name"))
  all_form_info <- purrr::map(all_form_info, "form_info")
  names(all_form_info) <- form_names

}


get_question_metadata <- function(form_info) {

  data.frame(
    title = form_info$form_metadata$items$title,
    paragraph = form_info$form_metadata$items$questionItem$question$textQuestion,
    choice_question = form_info$form_metadata$items$questionItem$question$choiceQuestion$type,
    required = form_info$form_metadata$items$questionItem$question$required
    )

}



clean_up_responses <- function(form_info) {

  metadata <- get_question_metadata(form_info)


  lapply(form_info)
  # row bind all question data together.
  question_info <- dplyr::bind_rows(responses, .id = "question_id")


  tmp <- tidyr::pivot_wider(question_info,
                            names_from = question_id,
                            values_from = answers)

}
clean_up_form_info <- function() {

}


extract_text_question <- function(question) {

    answers <- purrr::map(question$answers, ~.x$textAnswers$answers)

    answers <- purrr::map_depth(answers, 2, ~ifelse(is.null(.x),
                                                    data.frame(value = "NA"),
                                                    .x) )
    answers <- lapply(answers, purrr::map, -1)

    answers <- unlist(answers)

    answers_df <- data.frame(response_id = question$responseId,
                             answer = answers,
                             question_id = names(answers))

    return(answers_df)
}

extract_mc_question <- function(question) {

}
