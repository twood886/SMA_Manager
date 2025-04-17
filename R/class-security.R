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
#' @import Rblpapi
Security <- R6::R6Class( #nolint
  "Security",
  public = list(
    #' @description Create new Security R6 object
    #' @param id Character string. Security identifier (e.g., "AAPL").
    initialize = function(id = NULL) {
      stopifnot(is.character(id), length(id) == 1)
      private$id_ <- id
      private$description <- Rblpapi::bdp(id, "DX615")$DX615
      private$instrument_type <- Rblpapi::bdp(id, "EX028")$EX028
    },
    # Getters ------------------------------------------------------------------
    #' @description Get ID
    get_id = function() {
      return(private$id_)
    },
    #' @description Get Description
    get_description = function() {
      return(private$description)
    },
    #' @description Get Instrument Type
    get_instrument_type = function() {
      return(private$instrument_type)
    },
    #' @description Get Delta
    get_delta = function() {
      if (private$instrument_type == "Option") {
        delta <- (Rblpapi::bdp(private$id_, "OP006")$OP006)
      }else {
        delta <- 1
      }
      return(delta)
    },
    #' @description Get price
    get_price = function() {
      price <- Rblpapi::bdp(private$id_, "PX_LAST")$PX_LAST
      return(price)
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
    },
    #' Setters -----------------------------------------------------------------
    #' @description Add or overwrite an item in \code{data}
    #' @param ... Named values to store in \code{data} list.
    set_data_item = function(...) {
      args_list <- list(...)
      existing_keys <- intersect(names(args_list), names(private$data_))
      new_keys <- setdiff(names(args_list), names(private$data_))
      private$data_[existing_keys] <<- args_list[existing_keys]
      private$data_[new_keys] <<- args_list[new_keys]
    }
  ),
  private = list(
    id_ = NULL,
    description = NULL,
    instrument_type = NULL,
    delta = NULL,
    ts_data_ = list(),
    data_ = list()
  )
)