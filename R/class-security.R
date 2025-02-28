#' @title Security (R6 Class)
#'
#' @description
#' R6 class for a security with time-series and non-time-series data.
#'
#' @name Security
#' @rdname Security
#' @docType class
#'
#' @importFrom R6 R6Class
Security <- R6::R6Class( #nolint
  "Security",
  public = list(

    #' @description Create new Security R6 object
    #' @param id Character string. Security identifier (e.g., "AAPL").
    #' @param ts_data Named list of time-series data (like volume, prices).
    #' @param data Named list of non-time-series data.
    initialize = function(id = NULL, ts_data = NULL, data = NULL) {
      stopifnot(is.character(id), length(id) == 1)
      private$id_ <- id
      private$ts_data_ <- ts_data
      private$data_ <- data
    },
    #' @description Add or overwrite an item in \code{data}
    #' @param ... Named values to store in \code{data} list.
    set_data_item = function(...) {
      args_list <- list(...)
      existing_keys <- intersect(names(args_list), names(private$data_))
      new_keys <- setdiff(names(args_list), names(private$data_))
      private$data_[existing_keys] <<- args_list[existing_keys]
      private$data_[new_keys] <<- args_list[new_keys]
    },
    #' @description Get an items from \code{data} as a named value
    #' @param item String name of the list item in \code{data}
    get_data_item = function(item = NULL) {
      if (is.null(item)) stop("Item must be specified")
      if (!item %in% names(private$data_)) stop("Item not in data")
      return(private$data_[[item]])
    },
    #' @description Get an item from \code{ts_data} as a named vector.
    #' @param item String name of the time-series vector in \code{ts_data}.
    get_ts_data_item = function(item = NULL) {
      if (is.null(item)) stop("Item must be specified")
      if (!item %in% names(private$ts_data_)) stop("Item not in ts_data")
      ts <- private$ts_data_[item]
      return(ts)
    }
  ),
  private = list(
    id_ = NULL,
    ts_data_ = list(),
    data_ = list()
  )
)