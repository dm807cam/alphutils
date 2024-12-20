
#' @title Fetch rich list
#'
#' @description
#' Fetch addresses from explorer rich list.
#' This function iterates through the rich list and fetches all
#' addresses with their respective balances until no more addresses
#' with a balance >= `min_balance` can be found.
#'
#' @param endpoint **[character]**: URL to the explorer API
#' @param min_balance **[double]**: Min balance of an address to be included in the output
#' @param req_sleep **[integer]**: Number of seconds between requests. Important to deal with rate limits.
#' @inheritParams api_GET
#' @export
#' @return **[data.table]**: Rich list
#' @md
api_address_richlist <- function(
                         min_balance = 1,
                         endpoint = explorer_endpoint_select(),
                         req_timeout = 300,
                         req_sleep = 1) {
  checkmate::assert_numeric(
    min_balance,
    lower = 0,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_character(
    endpoint,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    req_sleep,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  page <- 1
  search_pages <- TRUE
  balance <- NULL
  resp_dt_list <- list()

  while (search_pages) {
    message("page: ", page)

    url <- file.path(endpoint,
                     paste0("tokens/holders/alph?page=", page, "&limit=100"))

    resp <- api_GET_with_retry(url = url, req_timeout = req_timeout)

    req_cols <- c("address", "balance")
    resp_list <- lapply(resp, \(x) {
      if (all(req_cols %in% names(x))) {
        data.table::as.data.table(x[req_cols])
      }
    })

    resp_dt <- data.table::rbindlist(resp_list)
    resp_dt[, balance := convert_pi_to_alph(balance)]

    resp_dt_list[[page]] <- resp_dt[balance >= min_balance, ]

    if (min(resp_dt$balance) < min_balance) {
      search_pages = FALSE
    } else {
      page <- page + 1
    }

    Sys.sleep(req_sleep)
  }

  address_dt <- data.table::rbindlist(resp_dt_list)

  return(address_dt)
}


#' @title Fetch number of locked ALPH
#'
#' @description
#' Fetch the number of locked $ALPH in a specific address.
#'
#' @param address **[character]**: A valid Alephium address
#' @param endpoint **[character]**: URL to the explorer API
#' @inheritParams api_GET
#' @export
#' @return **[double]**: Number of locked $ALPH
#' @md
api_address_locked_balance <- function(address,
                               endpoint = explorer_endpoint_select(),
                               req_timeout = 300) {
  checkmate::assert_character(
    address,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_character(
    endpoint,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  url <- file.path(endpoint, "addresses", address)

  resp <- api_GET_with_retry(url = url, req_timeout = req_timeout)

  return(as.numeric(resp$lockedBalance) * 1E-18)
}
