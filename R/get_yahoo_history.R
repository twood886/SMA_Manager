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

    data <- parsed_response[["chart"]][["result"]][[1]]
    match <- TRUE
    id <- data[["meta"]][["symbol"]]
    long_name <- data[["meta"]][["longName"]]
    short_name <- data[["meta"]][["shortName"]]
    dates <- as.Date(lubridate::as_datetime(unlist(data$timestamp)))
    open <- unlist(data[["indicators"]][["quote"]][[1]][["open"]])
    close <- unlist(data[["indicators"]][["quote"]][[1]][["close"]])
    low <- unlist(data[["indicators"]][["quote"]][[1]][["low"]])
    high <- unlist(data[["indicators"]][["quote"]][[1]][["high"]])
    volume <- unlist(data[["indicators"]][["quote"]][[1]][["volume"]])
    adj_close <- unlist(data[["indicators"]][["adjclose"]][[1]])
  } else {
    match <- FALSE
    id <- NA_character_
    long_name <- NA_character_
    short_name <- NA_character_
    dates <- lubridate::NA_Date_
    open <- NA_real_
    close <- NA_real_
    low <- NA_real_
    high <- NA_real_
    volume <- NA_real_
    adj_close <- NA_real_
  }

  list(
    "symbol" = as.character(symbol),
    "match" = match,
    "id" = as.character(id),
    "short_name" = as.character(short_name),
    "long_name" = as.character(long_name),
    "data" = data.frame(
      date = as.Date(dates),
      open = as.numeric(open),
      low = as.numeric(low),
      high = as.numeric(high),
      close = as.numeric(close),
      volume = as.numeric(volume),
      adj_close = as.numeric(adj_close)
    )
  )
}
