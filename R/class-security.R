# nolint start
#' @title Security (R6 Object)
#' @description
#' A security where data comes from yahoo finance
#'
#' @name Security
#' @rdname Security
#' @docType class
#'
#' @importFrom R6 R6Class
#' @include get_yahoo_history.R
#' @export
Security <- R6::R6Class(
  # nolint end
  "Security",
  public = list(
    #' @field ticker Security Ticker
    ticker = NULL,
    #' @field yahoo_match Boolean if ticker was found on yahoo finance
    yahoo_match = NULL,
    #' @field long_name Security Long Name
    long_name = NULL,
    #' @field short_name Security Short Name
    short_name = NULL,
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

    # Initialize Security object -----------------------------------------------
    #' @description Create New Security R6 object
    #'
    #' @param ticker Security Ticker
    #' @return A new \code{Security} object.
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
    },

    # Compute Average Daily Volume ---------------------------------------------
    #' @description Computes the average daily volume for the specified number
    #'  of trailing days.
    #'
    #' @param adv_days Integer. The number of days to consider.
    #' @return A numeric value representing the average daily volume.
    #'
    #' @details
    #' This method is added onto the \code{Security} R6 class dynamically.
    #'
    #' @examples
    #' \dontrun{
    #'   sec <- Security$new("AAPL")
    #'   sec$get_adv(30)
    #' }
    get_adv = function(adv_days = 30) {
      dates_all <- self$dates
      valid_dates <- dates_all[dates_all < Sys.Date()]
      volume_subset <- self$volume[match(valid_dates, sort(valid_dates))]
      len_volume <- length(volume_subset)
      mean(volume_subset[(len_volume - adv_days + 1):len_volume])
    }
  )
)