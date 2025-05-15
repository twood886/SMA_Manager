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
    delta_ = NULL
  ),
  public = list(
    #' @description Create new Security R6 object
    #' @param bbid Character string. Security identifier (e.g., "AAPL").
    #' @param description Character string. Security description.
    #' @param instrument_type Character string. Type of instrument (e.g., "Equity").
    #' @param price Numeric. Price of the security.
    #' @param delta Numeric. Delta of the security.
    initialize = function(bbid, description = NULL, instrument_type = NULL, price = NULL, delta = NULL) {
      assert_string(bbid, "bbid")
      private$bbid_ <- bbid

      if (!is.null(description)) {
        private$description_ <- description
      } else {
        private$description_ <- Rblpapi::bdp(bbid, "DX615")$DX615
      }

      if (!is.null(instrument_type)) {
        private$instrument_type_ <- instrument_type
      } else {
        private$instrument_type_ <- Rblpapi::bdp(bbid, "EX028")$EX028
      }

      if (!is.null(price)) {
        private$price_ <- price
      } else {
        private$price_ <- self$update_price()
      }

      if (!is.null(delta)) {
        private$delta_ <- delta
      } else {
        private$delta_ <- self$update_delta()
      }
    },
    # Getters ------------------------------------------------------------------
    #' @description Get ID
    get_id = function() private$bbid_,
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
      if (private$instrument_type_ == "FixedIncome") {
        price <- 1
      } else{
        price <- Rblpapi::bdp(private$bbid_, "PX_LAST")$PX_LAST
      }
      private$price_ <- price
      price
    },
    #' @description Update Delta
    update_delta = function() {
      if (private$instrument_type_ == "Option") {
        delta <- (Rblpapi::bdp(private$bbid_, "OP006")$OP006)
      }else {
        delta <- 1
      }
      private$delta_ <- delta
      delta
    }
  )
)
