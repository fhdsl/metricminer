# Extracting data from Google Forms

#' Get Google Forms
#' @description This is a function to get the Google Forms API requests.
#' The scopes it uses are the `See all your Google Forms forms.` and `See all responses to your Google Forms forms.`
#' If you don't check this box on the OAuth screen this function won't work.
#' @param url The endpoint URL for the request
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param body_params The body parameters for the request
#' @param query_params The body parameters for the request
#' @param return_request Should a list of the request be returned as well?
#' @returns This function returns a list from a API response JSON file
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
request_google_forms <- function(token, url,
                                 body_params = NULL,
                                 query_params = NULL,
                                 return_request = TRUE) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
  config <- httr::config(token = token)

  result <- httr::GET(
    url = url,
    body = body_params,
    query = query_params,
    config = config,
    httr::accept_json(),
    encode = "json"
  )

  request_info <- list(
    url = url,
    token = token,
    body_params = body_params,
    query_params = query_params
  )

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
#' @description This is a function to get Google Form info and responses from the API.
#' The scopes it uses are the `See all your Google Forms forms.` and `See all responses to your Google Forms forms.`
#' If you don't check this box on the OAuth screen this function won't work.
#' @param form_id The form ID we need to get
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param dataformat What format would you like the data? Options are "raw" or "dataframe". "dataframe" is the default.
#' @returns This returns a list of the form info and responses to the google form. Default is to make this a list of nicely formatted dataframes.
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' form_info <- get_google_form(
#'   "https://docs.google.com/forms/d/1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY/edit"
#' )
#' form_id <- "https://docs.google.com/forms/d/1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY/edit"
#'
#' ### OR You can give it a direct form id
#'
#' form_info <- get_google_form("1Neyj7wwNpn8wC7NzQND8kQ30cnbbETSpT0lKhX7uaQY")
#' }
get_google_form <- function(form_id, token = NULL, dataformat = "dataframe") {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "google")
  }
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
    token = token,
    return_request = TRUE
  )

  result <- list(
    form_metadata = form_info,
    response_info = response_info
  )

  if (dataformat == "dataframe") {
    metadata <- get_question_metadata(form_info)

    if (length(result$response_info$result) > 0) {
      answers_df <- extract_answers(result)
    } else {
      answers_df <- "no responses yet"
    }
    result <- list(
      title = result$form_metadata$result$info$title,
      metadata = metadata,
      answers = answers_df
    )
  }
  return(result)
}


#' Get multiple Google forms
#' @description This is a wrapper function for returning google form info and
#' responses for multiple forms at once. The scopes it uses are the `See all your Google Forms forms.`
#' and `See all responses to your Google Forms forms.`
#' If you don't check this box on the OAuth screen this function won't work.
#' @param form_ids a vector of form ids you'd like to retrieve information for
#' @param token credentials for access to Google using OAuth. `authorize("google")`
#' @param dataformat What format would you like the data? Options are "raw" or "dataframe". "dataframe" is the default.
#' @returns This returns a list of API information for google forms
#' @importFrom purrr map
#' @importFrom janitor make_clean_names
#' @export
#' @examples \dontrun{
#'
#' authorize("google")
#' form_list <- googledrive::drive_find(
#'   shared_drive = googledrive::as_id("0AJb5Zemj0AAkUk9PVA"),
#'   type = "form"
#' )
#'
#' multiple_forms <- get_multiple_forms(form_ids = form_list$id)
#' }
get_multiple_forms <- function(form_ids = NULL, token = NULL, dataformat = "dataframe") {
  # Get all the forms info
  all_form_info <- sapply(form_ids, function(form_id) {
    get_google_form(
      form_id = form_id,
      token = token,
      dataformat = dataformat
    )
  }, simplify = FALSE, USE.NAMES = TRUE)

  if (dataformat == "dataframe") {
    # Set up the names
    titles <- purrr::map(all_form_info, ~ .x$title)
    titles <- janitor::make_clean_names(titles)

    # Set as names
    names(all_form_info) <- titles
    }

  all_form_info
}

#' Google Form handling functions
#' @description This is a function to get metadata about a Google Form. It is
#'  used by the `get_google_form()` function if dataformat = "dataframe".
#' @param form_info The return form_info list that is extracted in `get_google_form()`
#' @returns This returns metadata from a google form
#' @export
get_question_metadata <- function(form_info) {
  metadata <- data.frame(
    question_id = form_info$result$items$itemId,
    title = form_info$result$items$title
  )

  if (length(form_info$result$items$questionItem$question$textQuestion) > 0) {
    metadata <- data.frame(
      metadata,
      paragraph = form_info$result$items$questionItem$question$textQuestion
    )
  }
  if (length(form_info$result$items$questionItem$question$choiceQuestion$type) > 0) {
    metadata <- data.frame(
      metadata,
      choice_question = form_info$result$items$questionItem$question$choiceQuestion$type,
      text_question = is.na(form_info$result$items$questionItem$question$choiceQuestion$type)
    )
  }

  return(metadata)
}

#' Google Form handling functions -- extracting answers
#' @description This is a function to get extract answers from a Google Form. It is
#'  used by the `get_google_form()` function if dataformat = "dataframe"
#' @param form_info The return form_info list that is extracted in `get_google_form()`
#' @export
#' @returns This returns answers from a google form
extract_answers <- function(form_info) {
  questions <- form_info$response_info$result$responses$answers

  if (length(questions) > 0) {
    # Extract the bits we want
    answers <- purrr::map(
      questions,
      ~ .x$textAnswers$answers
    )

    question_id <- purrr::map(
      questions,
      ~ .x$questionId
    )

    # Reformat the answer info
    answers <- purrr::map_depth(answers, 2, ~ ifelse(is.null(.x),
      data.frame(value = "NA"),
      .x
    ))

    answers <- purrr::map_depth(answers, -1, ~ ifelse(length(.x) > 1,
      paste0(.x, collapse = "|"),
      .x
    ))
    answers <- lapply(answers, purrr::map, -1)

    # Turn into data frames
    answers_df <- lapply(answers, paste0) %>% dplyr::bind_cols()

    colnames(answers_df) <- paste0(colnames(answers_df), "_answers")

    # Put it all in a data.frame we will keep
    info_df <- data.frame(
      response_id = rep(form_info$response_info$result$responses$responseId, length(questions)),
      answers_df
    )
  } else {
    info_df <- data.frame(value = "no responses yet")
  }

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
  body_params <- c(page_result$request_info$body_params,
    pageToken = page_result$result$nextPageToken
  )

  result <- request_google_forms(
    token = token,
    url = url,
    body_params = body_params,
    query_params = query_params,
    return_request = TRUE
  )

  return(result)
}
