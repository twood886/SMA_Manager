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
#' @export
Security <- R6::R6Class( #nolint
  "Security",
  private = list(
    bbid_ = NULL,
    description_ = NULL,
    instrument_type_ = NULL,
    price_ = NULL,
    delta_ = NULL,
  ),
  public = list(
    #' @description Create new Security R6 object
    #' @param id Character string. Security identifier (e.g., "AAPL").
    initialize = function(bbid = NULL) {
      asset_string(bbid, "bbid")
      private$bbid_ <- bbdid
      private$description_ <- Rblpapi::bdp(id, "DX615")$DX615
      private$instrument_type_ <- Rblpapi::bdp(id, "EX028")$EX028
      self$update_price()
      self$update_delta()
    },
    # Getters ------------------------------------------------------------------
    #' @description Get ID
    get_id = function() private$id_,
    #' @description Get Description
    get_description = function() private$description_,
    #' @description Get Instrument Type
    get_instrument_type = function() private$instrument_type_,
    #' @description Get Price
    get_price = function() private$price_,
    #' @description Get Delta
    get_delta = function() private$delta_,

    # Update Data --------------------------------------------------
    #' @description Update Price
    update_price = function() {
      price <- Rblpapi::bdp(private$id_, "PX_LAST")$PX_LAST
      private$price_ <- price
      price
    },
    #' @description Update Delta
    update_delta = function() {
      if (private$instrument_type_ == "Option") {
        delta <- (Rblpapi::bdp(private$id_, "OP006")$OP006)
      }else {
        delta <- 1
      }
      private$delta_ <- delta
      delta
    }
  )
)
