#' @title Create Security
#' @param ticker Security Ticker
#' @param adv_days ADV Days
#' @include security.R
#' @include get_yahoo_history.R
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#' @importFrom dplyr row_number
#' @importFrom dplyr pull
create_security_old <- function(ticker, adv_days = 30) {
  yahoo_data <- get_yahoo_history(ticker) #nolint

  if (yahoo_data$match) {
    adv <- yahoo_data$data %>%
      dplyr::filter(`date` < Sys.Date()) %>%
      dplyr::arrange(`date`) %>%
      dplyr::slice(tail(dplyr::row_number(), adv_days)) %>%
      dplyr::pull(`volume`) %>% #nolint
      mean()
  } else {
    adv <- NA_real_
  }

  new("security",
    ticker = ticker,
    yahoo_match = yahoo_data$match,
    long_name = yahoo_data$long_name,
    short_name = yahoo_data$short_name,
    adv = adv,
    dates = yahoo_data$data$date,
    open = yahoo_data$data$open,
    close = yahoo_data$data$close,
    low = yahoo_data$data$low,
    high = yahoo_data$data$high,
    volume = yahoo_data$data$volume,
    adj_close = yahoo_data$data$adj_close
  )
}

#' @title Create Security (R6 Object)
#' @description
#' Function to create an R6 Security Object
#' @param ticker Security Ticker
#' @include security.R
#' @export
create_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  # Check if the ticker already exists in the registry
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    stop("Security already exists")
  }
  # Create new security
  sec <- SecurityR6$new(ticker = ticker)
  assign(ticker, sec, envir = .security_registry)
}