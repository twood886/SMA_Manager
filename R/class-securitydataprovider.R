#' @title SecurityDataProvider (R6 Class)
#'
#' @description
#' Abstract interface for retrieving security reference and market data.
#' The core package classes (e.g. \code{\link{Security}}) request data through
#' this interface rather than calling a data vendor directly, so the domain
#' logic can run against any backend: Bloomberg (\code{BloombergDataProvider}),
#' in-memory data for tests and offline use (\code{StaticDataProvider}), or a
#' future database-backed implementation.
#'
#' Subclasses must implement all methods except \code{get_price} and
#' \code{get_delta}, which have default single-security implementations built
#' on their vectorized counterparts.
#'
#' @name SecurityDataProvider
#' @rdname SecurityDataProvider
#' @docType class
#'
#' @importFrom R6 R6Class
#' @export
SecurityDataProvider <- R6::R6Class( #nolint
  "SecurityDataProvider",
  public = list(
    #' @description Check whether a security exists in the data source.
    #' @param sec_id Character string. Security identifier.
    #' @return Logical.
    security_exists = function(sec_id) {
      stop("security_exists() not implemented for this provider.")
    },
    #' @description Get the security description.
    #' @param sec_id Character string. Security identifier.
    #' @return Character string.
    get_description = function(sec_id) {
      stop("get_description() not implemented for this provider.")
    },
    #' @description Get the instrument type (e.g. "Equity", "Option").
    #' @param sec_id Character string. Security identifier.
    #' @return Character string.
    get_instrument_type = function(sec_id) {
      stop("get_instrument_type() not implemented for this provider.")
    },
    #' @description Get the underlying security identifier for a derivative.
    #' @param sec_id Character string. Security identifier.
    #' @return Character string (ticker of the underlying, without the
    #'  " Equity" suffix).
    get_underlying_id = function(sec_id) {
      stop("get_underlying_id() not implemented for this provider.")
    },
    #' @description Get last prices for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    #' @return Named numeric vector of prices, names are \code{sec_ids}.
    get_prices = function(sec_ids) {
      stop("get_prices() not implemented for this provider.")
    },
    #' @description Get deltas for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    #' @return Named numeric vector of deltas, names are \code{sec_ids}.
    #'  May contain \code{NA} for securities without a delta.
    get_deltas = function(sec_ids) {
      stop("get_deltas() not implemented for this provider.")
    },
    #' @description Get arbitrary data fields for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    #' @param fields Character vector of field names (Bloomberg field
    #'  mnemonics).
    #' @return data.frame with one row per security (rownames are
    #'  \code{sec_ids}) and one column per field.
    get_fields = function(sec_ids, fields) {
      stop("get_fields() not implemented for this provider.")
    },
    #' @description Get the full reference record for a single security in
    #'  as few provider requests as the backend allows. Used by
    #'  \code{Security$new()} so creating a security does not cost one
    #'  round trip per field. The default implementation composes the
    #'  individual getters; backends with request latency should override
    #'  it with a single batched request.
    #' @param sec_id Character string. Security identifier.
    #' @return Named list with description, instrument_type, price, delta,
    #'  and underlying_id (underlying_id may be NULL/empty for
    #'  non-derivatives).
    get_security_profile = function(sec_id) {
      list(
        description     = self$get_description(sec_id),
        instrument_type = self$get_instrument_type(sec_id),
        price           = self$get_price(sec_id),
        delta           = self$get_delta(sec_id),
        underlying_id   = self$get_underlying_id(sec_id)
      )
    },
    #' @description Get the last price for a single security.
    #' @param sec_id Character string. Security identifier.
    #' @return Numeric.
    get_price = function(sec_id) {
      self$get_prices(sec_id)[[1]]
    },
    #' @description Get the delta for a single security.
    #' @param sec_id Character string. Security identifier.
    #' @return Numeric, possibly \code{NA}.
    get_delta = function(sec_id) {
      self$get_deltas(sec_id)[[1]]
    }
  )
)


#' @title BloombergDataProvider (R6 Class)
#'
#' @description
#' \code{\link{SecurityDataProvider}} implementation backed by the Bloomberg
#' Desktop API via \pkg{Rblpapi}. Requires a running Bloomberg terminal and a
#' connection established with \code{Rblpapi::blpConnect()}.
#'
#' @name BloombergDataProvider
#' @rdname BloombergDataProvider
#' @docType class
#'
#' @importFrom R6 R6Class
#' @export
BloombergDataProvider <- R6::R6Class( #nolint
  "BloombergDataProvider",
  inherit = SecurityDataProvider,
  public = list(
    #' @description Check whether a security exists in Bloomberg.
    #' @param sec_id Character string. Security identifier.
    security_exists = function(sec_id) {
      Rblpapi::bdp(sec_id, "DX194")$DX194 != ""
    },
    #' @description Get the security description (DX615).
    #' @param sec_id Character string. Security identifier.
    get_description = function(sec_id) {
      Rblpapi::bdp(sec_id, "DX615")$DX615
    },
    #' @description Get the instrument type (EX028).
    #' @param sec_id Character string. Security identifier.
    get_instrument_type = function(sec_id) {
      Rblpapi::bdp(sec_id, "EX028")$EX028
    },
    #' @description Get the underlying ticker for a derivative (DS492).
    #' @param sec_id Character string. Security identifier.
    get_underlying_id = function(sec_id) {
      Rblpapi::bdp(sec_id, "DS492")$DS492
    },
    #' @description Get last prices (PX_LAST) for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    get_prices = function(sec_ids) {
      px <- Rblpapi::bdp(sec_ids, "PX_LAST")
      out <- px$PX_LAST
      names(out) <- rownames(px)
      out
    },
    #' @description Get deltas (OP006) for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    get_deltas = function(sec_ids) {
      dl <- Rblpapi::bdp(sec_ids, "OP006")
      out <- dl$OP006
      names(out) <- rownames(dl)
      out
    },
    #' @description Get arbitrary Bloomberg fields for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    #' @param fields Character vector of Bloomberg field mnemonics.
    get_fields = function(sec_ids, fields) {
      Rblpapi::bdp(sec_ids, fields = fields)
    },
    #' @description Get the full reference record for a single security with
    #'  one Bloomberg request instead of one per field.
    #' @param sec_id Character string. Security identifier.
    get_security_profile = function(sec_id) {
      d <- Rblpapi::bdp(
        sec_id, c("DX615", "EX028", "PX_LAST", "OP006", "DS492")
      )
      list(
        description     = d$DX615,
        instrument_type = d$EX028,
        price           = d$PX_LAST,
        delta           = d$OP006,
        underlying_id   = d$DS492
      )
    }
  )
)


#' @title StaticDataProvider (R6 Class)
#'
#' @description
#' In-memory \code{\link{SecurityDataProvider}} implementation. Security data
#' is registered up front with \code{add_security()}, after which the full
#' provider interface works without any external connection. Intended for
#' unit tests and offline development.
#'
#' Identifiers are matched case-insensitively, consistent with the lowercasing
#' done by \code{\link{.security}}.
#'
#' @name StaticDataProvider
#' @rdname StaticDataProvider
#' @docType class
#'
#' @importFrom R6 R6Class
#' @import checkmate
#' @export
StaticDataProvider <- R6::R6Class( #nolint
  "StaticDataProvider",
  inherit = SecurityDataProvider,
  private = list(
    data_ = NULL,
    get_record = function(sec_id) {
      rec <- private$data_[[tolower(sec_id)]]
      if (is.null(rec)) {
        stop("StaticDataProvider: no data for security '", sec_id, "'")
      }
      rec
    }
  ),
  public = list(
    #' @description Create a new StaticDataProvider.
    initialize = function() {
      private$data_ <- list()
    },
    #' @description Register (or replace) data for a security.
    #' @param sec_id Character string. Security identifier.
    #' @param description Character string. Security description.
    #' @param instrument_type Character string. Instrument type
    #'  (e.g. "Equity", "Option").
    #' @param price Numeric. Last price.
    #' @param delta Numeric. Delta (defaults to \code{NA}; non-options
    #'  fall back to 1 in \code{Security}).
    #' @param underlying_id Character string. Underlying ticker for
    #'  derivatives, without the " Equity" suffix.
    #' @param fields Named list of additional field values, keyed by field
    #'  mnemonic (used by \code{get_fields()}).
    #' @return The provider, invisibly (allows chaining).
    add_security = function(
      sec_id, description = sec_id, instrument_type = "Equity",
      price = NA_real_, delta = NA_real_, underlying_id = NULL,
      fields = list()
    ) {
      checkmate::assert_character(sec_id, len = 1)
      checkmate::assert_character(description, len = 1)
      checkmate::assert_character(instrument_type, len = 1)
      checkmate::assert_numeric(price, len = 1)
      checkmate::assert_numeric(delta, len = 1)
      checkmate::assert_character(underlying_id, len = 1, null.ok = TRUE)
      checkmate::assert_list(fields, names = "unique")
      private$data_[[tolower(sec_id)]] <- list(
        description = description,
        instrument_type = instrument_type,
        price = price,
        delta = delta,
        underlying_id = underlying_id,
        fields = fields
      )
      invisible(self)
    },
    #' @description Update the stored price for a security.
    #' @param sec_id Character string. Security identifier.
    #' @param price Numeric. New price.
    set_price = function(sec_id, price) {
      rec <- private$get_record(sec_id)
      rec$price <- price
      private$data_[[tolower(sec_id)]] <- rec
      invisible(self)
    },
    #' @description Check whether a security has been registered.
    #' @param sec_id Character string. Security identifier.
    security_exists = function(sec_id) {
      tolower(sec_id) %in% names(private$data_)
    },
    #' @description Get the security description.
    #' @param sec_id Character string. Security identifier.
    get_description = function(sec_id) {
      private$get_record(sec_id)$description
    },
    #' @description Get the instrument type.
    #' @param sec_id Character string. Security identifier.
    get_instrument_type = function(sec_id) {
      private$get_record(sec_id)$instrument_type
    },
    #' @description Get the underlying ticker for a derivative.
    #' @param sec_id Character string. Security identifier.
    get_underlying_id = function(sec_id) {
      private$get_record(sec_id)$underlying_id
    },
    #' @description Get prices for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    get_prices = function(sec_ids) {
      out <- vapply(
        sec_ids, function(id) private$get_record(id)$price, numeric(1)
      )
      names(out) <- sec_ids
      out
    },
    #' @description Get deltas for a vector of securities.
    #' @param sec_ids Character vector of security identifiers.
    get_deltas = function(sec_ids) {
      out <- vapply(
        sec_ids, function(id) private$get_record(id)$delta, numeric(1)
      )
      names(out) <- sec_ids
      out
    },
    #' @description Get registered field values for a vector of securities.
    #' The canonical price and delta mnemonics (PX_LAST, OP006) are served
    #' from the registered price and delta, matching how a Bloomberg-backed
    #' provider responds to those fields.
    #' @param sec_ids Character vector of security identifiers.
    #' @param fields Character vector of field mnemonics.
    get_fields = function(sec_ids, fields) {
      cols <- lapply(fields, function(f) {
        sapply(sec_ids, function(id) {
          rec <- private$get_record(id)
          if (identical(f, "PX_LAST")) return(rec$price)
          if (identical(f, "OP006")) return(rec$delta)
          value <- rec$fields[[f]]
          if (is.null(value)) NA else value
        })
      })
      df <- data.frame(cols, stringsAsFactors = FALSE)
      colnames(df) <- fields
      rownames(df) <- sec_ids
      df
    }
  )
)


#' @title Set the Active Security Data Provider
#' @description Sets the provider used by \code{\link{Security}} and related
#' functions to fetch security data. Call this once at startup to run against
#' a non-default backend (e.g. a \code{\link{StaticDataProvider}} in tests).
#' @param provider A \code{\link{SecurityDataProvider}} object.
#' @return The provider, invisibly.
#' @import checkmate
#' @export
set_security_data_provider <- function(provider) {
  checkmate::assert_r6(provider, "SecurityDataProvider")
  .pkg_state$security_data_provider <- provider
  invisible(provider)
}


#' @title Get the Active Security Data Provider
#' @description Returns the provider set by
#' \code{\link{set_security_data_provider}}. If none has been set, a
#' \code{\link{BloombergDataProvider}} is created, cached, and returned.
#' @return A \code{\link{SecurityDataProvider}} object.
#' @export
get_security_data_provider <- function() {
  provider <- .pkg_state$security_data_provider
  if (is.null(provider)) {
    provider <- BloombergDataProvider$new()
    .pkg_state$security_data_provider <- provider
  }
  provider
}
