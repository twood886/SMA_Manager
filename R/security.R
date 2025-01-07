#' @title Security (S4 Object)
#' @description An S4 Class to represent a security
#' @slot ticker Security Ticker
#' @slot yahoo_match Boolean if found on yahoo finance
#' @slot long_name Security Long Name
#' @slot short_name Security Short Name
#' @slot adv Average Daily Volume
#' @slot dates Dates corresponding to data
#' @slot open Open Prices
#' @slot close Close Prices
#' @slot low Low Prices
#' @slot high High Prices
#' @slot volume Trading Volume
#' @slot adj_close Adjusted Close Price
setClass(
  "security",
  representation(
    ticker = "character",
    yahoo_match = "logical",
    long_name = "character",
    short_name = "character",
    adv = "numeric",
    dates = "Date",
    open = "numeric",
    close = "numeric",
    low = "numeric",
    high = "numeric",
    volume = "numeric",
    adj_close = "numeric"
  )
)


#' @title Security (R6 Object)
#' @docType class
#' @description
#' A security where data comes from yahoo finance
#' @import R6
SecurityR6 <- R6::R6Class("SecurityR6",
  public = list(
    #' @field ticker Security Ticker
    ticker = NULL,
    #' @field yahoo_match Boolean if ticker was found on yahoo finance
    yahoo_match = NULL,
    #' @field long_name Security Long Name
    long_name = NULL,
    #' @field short_name Security Short Name
    short_name = NULL,
    #' @field adv Average Daily Volume
    adv = NULL,
    #' @field dates Dates corresponding to data
    dates = NULL,
    #' @field open Open Prices
    open = NULL,
    #' @field close Close Prices
    close = NULL,
    #' @field low Low Prices
    low = NULL,
    #' @field high High Prices
    high = NULL,
    #' @field volume Trading Volume
    volume = NULL,
    #' @field adj_close Adjusted Close Price
    adj_close = NULL,

    #' @description
    #' Create New SecurityR6 object
    #' @param ticker Security Ticker
    #' @return A new `SecurityR6` object.
    initialize = function(ticker) {
      yahoo_data <- get_yahoo_history(ticker)
      self$ticker <- ticker
      self$yahoo_match <- yahoo_data$yahoo_match
      self$long_name <- yahoo_data$long_name
      self$short_name <- yahoo_data$short_name
      self$dates <- yahoo_data$data$date
      self$open <- yahoo_data$data$open
      self$close <- yahoo_data$data$close
      self$low <- yahoo_data$data$low
      self$high <- yahoo_data$data$high
      self$volume <- yahoo_data$data$volume
      self$adj_close <- yahoo_data$data$adj_close
    }
  )
)