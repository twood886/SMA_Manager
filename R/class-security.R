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
    #' @field ts_data Time Series Data List
    ts_data = list(
      dates = NULL,
      open = NULL,
      close = NULL,
      low = NULL,
      high = NULL,
      volume = NULL,
      adj_close = NULL
    ),
    #' @field data Non-Time Series Data List
    data = list(NULL),

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
      self$ts_data$dates <- yahoo_data$data$date
      self$ts_data$open <- yahoo_data$data$open
      self$ts_data$close <- yahoo_data$data$close
      self$ts_data$low <- yahoo_data$data$low
      self$ts_data$high <- yahoo_data$data$high
      self$ts_data$volume <- yahoo_data$data$volume
      self$ts_data$adj_close <- yahoo_data$data$adj_close
    },

    # Get TS Data Item ---------------------------------------------------------
    #' @description Gets an item from the ts_data list field.
    #'
    #' @param item String. Item in ts_data list.
    #' @return An array of ts_data with dates as name.
    get_ts_data_item = function(item = NULL) {
      if (is.null(item)) {
        stop("Item must be specified")
      }
      if (!item %in% names(self$ts_data)) {
        stop("Item not in ts_data")
      }
      ts <- self$ts_data[[item]]
      names(ts) <- self$ts_data$dates
      return(ts)
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
      dates_all <- self$ts_data$dates
      valid_dates <- dates_all[dates_all < Sys.Date()]
      volume_subset <- self$ts_data$volume[
        match(valid_dates, sort(valid_dates))
      ]
      len_volume <- length(volume_subset)
      mean(volume_subset[(len_volume - adv_days + 1):len_volume])
    }
  )
)