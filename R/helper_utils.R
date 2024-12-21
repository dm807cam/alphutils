#' @title Convert Pi to ALPH
#'
#' @param pi **[double]**: Amount of Pi converted to $ALPH
#' @return **[double]**: $ALPH
convert_pi_to_alph <- function(pi) {
  pi <- as.numeric(pi)

  checkmate::assert_numeric(
    pi,
    lower = 1,
    null.ok = FALSE
  )

  return(pi * 1E-18)
}


#' @title Get current UTC date and time
#'
#' @return **[POSIXct]**: Value
#' @md
utc_time <- function() {
  return(as.POSIXct(Sys.time(), tz = "UTC"))
}


#' @title Fetch random number(s)
#'
#' @description Fetch truly random number(s) from random.org
#'
#' @param n **[integer]**: Length of random vector
#' @param min **[integer]**: Min number to consider by random.org
#' @param max **[integer]**: Max number to consider by random.org
#' @param max_retries **[integer]**: Max number of retries if random.org returns an invalid repsonse
#' @export
#' @return **[integer]**: Vector
#' @md
true_random <- function(n = 1, min = 1, max = 100, max_retries = 5) {
  checkmate::assert_integerish(
    n,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    min,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    max,
    lower = 2,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    max_retries,
    lower = 1,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )

  rtry <- 0
  while (rtry <= max_retries) {
    tryCatch(
      expr = {
        resp <- random::randomNumbers(
          n = n,
          min = min,
          max = max,
          col = 1
        ) |> as.integer()
        rtry <- max_retries + 1
      },
      error = function(e) {
        rtry <- rtry + 1
        warning("Issue fetching random.org. Retry: ",
          rtry,
          " of ",
          max_retries,
          immediate. = TRUE
        )
        Sys.sleep(60 * rtry)
      }
    )
  }
  return(resp)
}
