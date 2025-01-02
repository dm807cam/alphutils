#' @title GET request
#'
#' @param url **[character]**: URL to the API endpoint
#' @param req_timeout **[integer]**: Number of seconds until request times out
#' @return **[list]**: API response content
#' @md
api_GET <- function(url, req_timeout) {
  checkmate::assert_character(url,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    req_timeout,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  get <- httr::GET(url = url, httr::timeout(req_timeout))
  stopifnot(httr::status_code(get) == 200)

  resp <- httr::content(get)

  return(resp)
}

#' @title GET request with retry
#'
#' @description
#' Wrapper function to `api_GET()` that retries `max_retries` times if a request failed.
#'
#' @inheritParams api_GET
#' @param max_retries **[integer]**: Number of retries before the function fails
#' @return **[list]**: API response content
#' @md
api_GET_with_retry <- function(url, req_timeout, max_retries = 5) {
  checkmate::assert_character(url,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    req_timeout,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    max_retries,
    lower = 0,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  rtry <- 0
  while (rtry <= max_retries) {
    tryCatch(
      expr = {
        resp <- api_GET(url = url, req_timeout = req_timeout)
        rtry <- max_retries + 1
      },
      error = function(e) {
        if (rtry <= max_retries) {
          rtry <<- rtry + 1
          warning("Issue fetching from ",
            url,
            " Retry: ",
            rtry,
            " of ",
            max_retries,
            immediate. = TRUE
          )
          Sys.sleep(10 * rtry)
        } else {
          stop("Persistent issues fetching from ", url, " after ", max_retries, "attempts.", immediate. = TRUE)
        }
      }
    )
  }
  return(resp)
}
