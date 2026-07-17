#' @title Security (R6 Class)
#'
#' @description
#' R6 class for a security with time-series and non-time-series data.
#'
#' All external security data is fetched through the active
#' \code{\link{SecurityDataProvider}} (see
#' \code{\link{set_security_data_provider}}), never from a vendor directly.
#'
#' @name Security
#' @rdname Security
#' @docType class
#'
#' @importFrom R6 R6Class
#' @import checkmate
#' @export
Security <- R6::R6Class( #nolint
  "Security",
  private = list(
    bbid_ = NULL,
    description_ = NULL,
    instrument_type_ = NULL,
    price_ = NULL,
    delta_ = NULL,
    underlying_security_ = NULL,
    rule_data_ = list(NULL)
  ),
  public = list(
    #' @description Create new Security R6 object
    #' @param bbid Character string. Security identifier (e.g., "AAPL").
    #' @param description Character string. Security description.
    #' @param instrument_type Character string. Type of instrument.
    #' @param price Numeric. Price of the security.
    #' @param underlying_security Security. Underlying security for options.
    #' @param delta Numeric. Delta of the security.
    initialize = function(
      bbid, description = NULL, instrument_type = NULL,
      price = NULL, delta = NULL, underlying_security = NULL
    ) {
      checkmate::assert_character(bbid)
      private$bbid_ <- bbid

      # Anything not supplied comes from the provider. Fetch it all in one
      # request (get_security_profile) rather than one round trip per field.
      profile <- if (
        is.null(description) || is.null(instrument_type) ||
          is.null(price) || is.null(delta)
      ) {
        get_security_data_provider()$get_security_profile(bbid)
      }

      private$description_ <- description %||% profile$description
      private$instrument_type_ <- instrument_type %||% profile$instrument_type

      if (!is.null(price)) {
        private$price_ <- price
      } else if (identical(private$instrument_type_, "FixedIncome")) {
        private$price_ <- 1
      } else {
        private$price_ <- profile$price
      }

      if (!is.null(delta)) {
        private$delta_ <- delta
      } else {
        d <- if (.is_option_type(private$instrument_type_)) {
          profile$delta
        } else {
          1
        }
        if (is.null(d) || length(d) == 0 || !is.finite(d)) d <- 1
        private$delta_ <- d
      }

      if (.is_option_type(private$instrument_type_)) {
        if(!is.null(underlying_security)) {
          checkmate::assert_r6(underlying_security, "Security")
          private$underlying_security_ <- underlying_security
        } else {
          # profile is NULL when all fields were supplied; fall back to the
          # single-field provider lookup for the underlying id.
          underlying_id <- profile$underlying_id %||%
            get_security_data_provider()$get_underlying_id(bbid)
          underlying_sec <- .security(paste0(underlying_id, " Equity"))
          checkmate::assert_r6(underlying_sec, "Security")
          private$underlying_security_ <- underlying_sec
        }
      } else {
        private$underlying_security_ <- NULL
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
    #' @description Get Underlying Security
    get_underlying_security = function() {
      if (is.null(private$underlying_security_)) return(self)
      private$underlying_security_
    },
    #' @description Get Underlying Price
    get_underlying_price = function() {
      if (is.null(private$underlying_security_)) return(self$get_price())
      private$underlying_security_$get_price()
    },
    #' @description Get Delta
    get_delta = function() private$delta_,
    #' @description Get Delta-Adjusted Price (Delta * Underlying Price)
    get_delta_price = function() {
      self$get_delta() * self$get_underlying_price()
    },
    #' @description Get Rule Data
    #' @param bbfield Character. bbfield.
    #' @param underlying Logical. If TRUE and the security is an option, get the
    #'  data from the underlying security. Defaults to FALSE.
    get_rule_data = function(bbfield, underlying = FALSE) {
      checkmate::assert_character(bbfield)
      if (isTRUE(underlying) && .is_option_type(private$instrument_type_)) {
        return(private$underlying_security_$get_rule_data(bbfield))
      }
      if (bbfield %in% names(private$rule_data_)) {
        return(private$rule_data_[[bbfield]])
      } else {
        stop(paste("bbfield", bbfield, "not found in security data."))
      }
    },
    # Update Data --------------------------------------------------
    #' @description Update Prices & Delta
    update_prices_delta = function() {
      self$update_price()
      self$update_delta()
      self$update_underlying_price()
      invisible(TRUE)
    },
    #' @description Update Price
    update_price = function() {
      if (private$instrument_type_ == "FixedIncome") {
        price <- 1
      } else {
        price <- get_security_data_provider()$get_price(private$bbid_)
      }
      private$price_ <- price
      invisible(price)
    },
    #' @description Update Delta
    update_delta = function() {
      delta <- NULL
      if (.is_option_type(private$instrument_type_)) {
        delta <- get_security_data_provider()$get_delta(private$bbid_)
      }
      if (is.null(delta) || !is.finite(delta)) delta <- 1
      private$delta_ <- delta
      invisible(delta)
    },
    #' @description Update Underlying Price
    update_underlying_price = function() {
      underlying_price <- NULL
      if (.is_option_type(self$get_instrument_type())) {
        if (!is.null(self$get_underlying_security())) {
          underlying_price <- private$underlying_security_$update_price()
        }
      }
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
