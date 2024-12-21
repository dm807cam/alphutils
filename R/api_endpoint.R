#' @title Explorer API URL
#'
#' @description
#' Supplies a number of different explorer API endpoints.
#'
#' @param ii **[integer]**: Index of from internal URL vector to be returned. This is useful if multiple URL ought to be tried sequentially.
#' @return **[character]**: Explorer API URL
#' @md
explorer_endpoint_select <- function(ii = 1) {
  checkmate::assert_integerish(
    ii,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  ee <- c("https://backend.mainnet.alephium.org")

  if (ii > length(ee)) {
    stop(
      "Larger index than API URL vector was selected.\n\tMust choose one of:",
      paste0("\n\t", seq_len(length(ee)), " ", ee)
    )
  }

  return(ee[ii])
}
