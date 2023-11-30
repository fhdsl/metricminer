# Extracting data from Google Forms

#' Get Google Forms
#' @description This is a function to get the Calendly API user info
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query_params The body parameters for the request
#' @param return_request Should a list of the request be returned as well?
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
request_google_forms <- function(token, url, query = NULL, body_params = NULL, query_params = NULL,
                                 return_request = TRUE) {
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

  request_info <- list(url = url,
                       token = token,
                       body_params = body_params,
                       query_params = query_params)

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
    return(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  if (return_request) {
    return(list(result = result_list, request_info = request_info))
  } else {
    return(result_list)
  }

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
  if (grepl("https:", form_id[1])) {
    form_id <- gsub("\\/viewform$|\\/edit$", "", form_id)
    form_id <- gsub("https://docs.google.com/forms/d/e/|https://docs.google.com/forms/d/", "", form_id)
  }

  form_info_url <- gsub("\\{formId\\}", form_id, "https://forms.googleapis.com/v1/forms/{formId}")
  form_response_url <- gsub("\\{formId\\}", form_id, "https://forms.googleapis.com/v1/forms/{formId}/responses")

  message(paste0("Trying to grab form: ", form_id))

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
#' multiple_forms <- get_multiple_forms(form_ids = form_list$id)
#'
#' }

get_multiple_forms <- function(form_ids = NULL, token = NULL) {

  all_form_info <- lapply(form_ids, function(form_id) {
    form_id <- "13XGFFvVdOkIs2RjpkMWVsh9DEV0MsPalk9Fevu8IyOg"
    form_info <- get_google_form(
      form_id = form_id,
      token = token
      )

    if (length(form_info$response_info) > 0 ) {

      metadata <- get_question_metadata(form_info)

      answers_df <- extract_answers(form_info)

      return(answers_df)
    } else {
      return("No responses to this form yet.")
    }
  })

  all_form_info

  return(all_form_info)
}

get_question_metadata <- function(form_info) {

  metadata <- data.frame(
    question_id = form_info$form_metadata$result$items$itemId,
    title = form_info$form_metadata$result$items$title
    )

  if (length(form_info$form_metadata$result$items$questionItem$question) > 0 ) {
  metadata <- data.frame(
    metadata,
    paragraph = form_info$form_metadata$result$items$questionItem$question$textQuestion,
    choice_question = form_info$form_metadata$result$items$questionItem$question$choiceQuestion$type,
    text_question = form_info$form_metadata$result$items$questionItem$question$choiceQuestion$type
    )
  }

  return(metadata)
}

extract_answers <- function(form_info) {

    questions <- form_info$response_info$result$responses$answers

    # Extract the bits we want
    answers <- purrr::map(questions,
                          ~.x$textAnswers$answers)

    question_id <- purrr::map(questions,
                              ~.x$questionId)

    # Reformat the answer info
    answers <- purrr::map_depth(answers, 2, ~ifelse(is.null(.x),
                                                    data.frame(value = "NA"),
                                                    .x) )

    answers <- purrr::map_depth(answers, -1, ~ifelse(length(.x) > 1,
                                                    paste0(.x, collapse = "|"),
                                                    .x) )
    answers <- lapply(answers, purrr::map, -1)

    # Turn into data frames
    answers_df <- lapply(answers, paste0) %>% dplyr::bind_cols()
    question_df <- lapply(question_id, paste0) %>% dplyr::bind_cols()

    colnames(answers_df) <- paste0(colnames(answers_df), "_answers")
    colnames(question_df) <- paste0(colnames(question_df), "_question")

    # Put it all in a data.frame we will keep
    info_df <- data.frame(
      reponse_id = rep(form_info$response_info$result$responses$responseId, length(questions)),
      answers_df,
      question_df
    )

    return(info_df)
}


google_pagination <- function(first_page_result) {
  # Set up a while loop for us to store the multiple page requests in
  cummulative_pages <- first_page_result$result$files
  page <- 1

  next_pg <- try(next_google(first_page_result), silent = TRUE)

  while (!grepl("Error", next_pg$result[1])) {
    cummulative_pages <- dplyr::bind_rows(cummulative_pages, next_pg$result$files)
    next_pg <- try(next_google(first_page_result), silent = TRUE)
    page <- page + 1
  }
  return(cummulative_pages)
}


next_google <- function(page_result) {

  ## TODO: Next page request is not working! Not sure why. It doesn't throw an error,
  ## but it just gives the same result everytime!
  body_params <- c(page_result$request_info$body_params,
                   pageToken = page_result$result$nextPageToken)

  result <- request_google_forms(
    token = token,
    url = url,
    body_params = body_params,
    query_params = query_params,
    return_request = TRUE
  )

  return(result)
}
