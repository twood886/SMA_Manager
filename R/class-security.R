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
#' @import checkmate
#' @export
Security <- R6::R6Class( #nolint
  "Security",
  private = list(
    bbid_ = NULL,
    description_ = NULL,
    instrument_type_ = NULL,
    price_ = NULL,
    underlying_price_ = NULL,
    delta_ = NULL,
    bics_level_2_ = NULL,
    bics_level_3_ = NULL,
    rule_data_ = list(NULL)
  ),
  public = list(
    #' @description Create new Security R6 object
    #' @param bbid Character string. Security identifier (e.g., "AAPL").
    #' @param description Character string. Security description.
    #' @param instrument_type Character string. Type of instrument.
    #' @param price Numeric. Price of the security.
    #' @param underlying_price Numeric. Price of underlying security.
    #' @param delta Numeric. Delta of the security.
    #' @param bics_level_2 Character string. BICS Level 2 classification.
    #' @param bics_level_3 Character string. BICS Level 3 classification.
    initialize = function(
      bbid, description = NULL, instrument_type = NULL,
      price = NULL, underlying_price = NULL, delta = NULL,
      bics_level_2 = NULL, bics_level_3 = NULL
    ) {
      checkmate::assert_character(bbid)
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

      if (!is.null(underlying_price)) {
        private$underlying_price_ <- underlying_price
      } else {
        private$underlying_price_ <- self$update_underlying_price()
      }

      if (!is.null(delta)) {
        private$delta_ <- delta
      } else {
        private$delta_ <- self$update_delta()
      }

      if (!is.null(bics_level_2)) {
        private$bics_level_2_ <- bics_level_2
      } else {
        private$bics_level_2_ <- Rblpapi::bdp(bbid, "BI012")$BI012
      }

      if (!is.null(bics_level_3)) {
        private$bics_level_3_ <- bics_level_3
      } else {
        private$bics_level_3_ <- Rblpapi::bdp(bbid, "BI013")$BI013
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
    #' @description Get Underlying Price
    get_underlying_price = function() private$underlying_price_,
    #' @description Get Delta
    get_delta = function() private$delta_,
    #' @description Get BICS Level 2
    get_bics_level_2 = function() private$bics_level_2_,
    #' @description Get BICS Level 3
    get_bics_level_3 = function() private$bics_level_3_,
    #' @description Get Rule Data
    #' @param bbfield Character. bbfield.
    get_rule_data = function(bbfield) {
      checkmate::assert_character(bbfield)
      if (bbfield %in% names(private$rule_data_)) {
        private$rule_data_[[bbfield]]
      } else {
        stop(paste("bbfield", bbfield, "not found in security data."))
      }
    },
    # Update Data --------------------------------------------------
    #' @description Update Price
    update_price = function() {
      if (private$instrument_type_ == "FixedIncome") {
        price <- 1
      } else {
        price <- Rblpapi::bdp(private$bbid_, "PX_LAST")$PX_LAST
      }
      private$price_ <- price
      invisible(price)
    },
    #' @description Update Delta
    update_delta = function() {
      if (private$instrument_type_ == "Option") {
        delta <- (Rblpapi::bdp(private$bbid_, "OP006")$OP006)
      }else {
        delta <- 1
      }
      private$delta_ <- delta
      invisible(delta)
    },
    #' @description Update Underlying Price
    update_underlying_price = function() {
      if (private$instrument_type_ == "Option") {
        underlying_price <- Rblpapi::bdp(private$bbid_, "OP004")$OP004
      } else {
        underlying_price <- private$price_
      }
      private$underlying_price_ <- underlying_price
      invisible(underlying_price)
    },

    # Setters ------------------------------------------------------------------
    #' @description Set Price
    #' @param price Numeric. New price of the security.
    set_price = function(price) {
      checkmate::assert_numeric(price)
      private$price_ <- price
      invisible(TRUE)
    },
    #' @description Set Delta
    #' @param delta Numeric. New delta of the security.
    set_delta = function(delta) {
      checkmate::assert_numeric(delta)
      private$delta_ <- delta
      invisible(TRUE)
    },
    #' @description Set Rule Data
    #' @param bbfield Character. Name of the rule.
    #' @param data value
    set_rule_data = function(bbfield, data) {
      checkmate::assert_character(bbfield)
      private$rule_data_[[bbfield]] <- data
      invisible(TRUE)
    }
  )
)
