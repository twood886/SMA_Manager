#' @title Get Yahoo Finance Historical Data
#' @param symbol Ticker Symbol Used on Yahoo Finance
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime
#' @importFrom dplyr filter
#' @importFrom tidyr complete
#' @importFrom tidyr fill
get_yahoo_history <- function(symbol = NULL) {
  if (is.null(symbol)) stop("Symbol cannot be NULL")

  yahoo_api_path <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    symbol,
    "?range=1y&interval=1d"
  )

  yahoo_resp <- httr::GET(yahoo_api_path)

  if (yahoo_resp$status_code == 200) {
    parsed_response <- jsonlite::fromJSON(
      httr::content(yahoo_resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )

    null_2_na <- function(x) {
      x[sapply(x, is.null)] <- NA
      return(x)
    }

    data <- parsed_response[["chart"]][["result"]][[1]]
    dates <- as.Date(lubridate::as_datetime(unlist(data$timestamp)))
    open <- unlist(null_2_na(data[["indicators"]][["quote"]][[1]][["open"]]))
    close <- unlist(null_2_na(data[["indicators"]][["quote"]][[1]][["close"]]))
    low <- unlist(null_2_na(data[["indicators"]][["quote"]][[1]][["low"]]))
    high <- unlist(null_2_na(data[["indicators"]][["quote"]][[1]][["high"]]))
    volume <- unlist(null_2_na(data[["indicators"]][["quote"]][[1]][["volume"]])) #nolint
    adj_close <- unlist(null_2_na(data[["indicators"]][["adjclose"]][[1]][["adjclose"]])) #nolint
  } else {
    dates <- lubridate::NA_Date_
    open <- NA_real_
    close <- NA_real_
    low <- NA_real_
    high <- NA_real_
    volume <- NA_real_
    adj_close <- NA_real_
  }

  list(
    "open" = setNames(as.numeric(open), dates),
    "low" = setNames(as.numeric(low), dates),
    "high" = setNames(as.numeric(high), dates),
    "close" = setNames(as.numeric(close), dates),
    "volume" = setNames(as.numeric(volume), dates),
    "adj_close" = setNames(as.numeric(adj_close), dates)
  )
}
