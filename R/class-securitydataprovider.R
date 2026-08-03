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


#' @title CompositeDataProvider (R6 Class)
#'
#' @description
#' \code{\link{SecurityDataProvider}} that serves a small set of manually
#' registered "override" securities itself and delegates every other security
#' to a \code{primary} provider (typically \code{\link{BloombergDataProvider}}).
#'
#' The motivating case is an OTC option that has no tradeable identifier in the
#' primary data source: its delta is supplied out of band (e.g. from Enfusion)
#' and its price is derived from the live price of its underlying, which the
#' primary provider \emph{can} price. Every other security passes straight
#' through, so a single batched request (e.g. from
#' \code{\link{update_security_data}}) that mixes override and primary ids is
#' split, served from both sources, and recombined.
#'
#' @name CompositeDataProvider
#' @rdname CompositeDataProvider
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stats setNames
#' @import checkmate
#' @export
CompositeDataProvider <- R6::R6Class( #nolint
  "CompositeDataProvider",
  inherit = SecurityDataProvider,
  private = list(
    primary_ = NULL,
    overrides_ = NULL,
    get_override = function(sec_id) private$overrides_[[tolower(sec_id)]],
    is_override = function(sec_id) {
      tolower(sec_id) %in% names(private$overrides_)
    },
    # Price for one override, derived from its underlying's primary price.
    derive_override_price = function(ov) {
      u_px <- private$primary_$get_prices(ov$underlying_id)[[1]]
      ov$derive_price(u_px, ov$delta)
    }
  ),
  public = list(
    #' @description Create a new CompositeDataProvider.
    #' @param primary A \code{\link{SecurityDataProvider}} to delegate to for
    #'  any security not registered as an override.
    initialize = function(primary) {
      checkmate::assert_r6(primary, "SecurityDataProvider")
      private$primary_ <- primary
      private$overrides_ <- list()
    },
    #' @description The primary (delegated-to) provider.
    get_primary = function() private$primary_,
    #' @description Register (or replace) an override security served locally
    #'  instead of by \code{primary}.
    #' @param sec_id Character. Security identifier (matched case-insensitively).
    #' @param underlying_id Character. Underlying identifier, priced via
    #'  \code{primary}.
    #' @param delta Numeric. Signed delta (puts negative).
    #' @param instrument_type Character. Defaults to "OTC Option".
    #' @param description Character. Defaults to \code{sec_id}.
    #' @param derive_price Function \code{(underlying_price, delta)} returning
    #'  the override's price. Defaults to \code{abs(delta) * underlying_price}.
    #' @param fields Named list of extra field values keyed by mnemonic.
    #' @return The provider, invisibly (allows chaining).
    register_otc_option = function(
      sec_id, underlying_id, delta,
      instrument_type = "OTC Option", description = sec_id,
      derive_price = NULL, fields = list()
    ) {
      checkmate::assert_string(sec_id)
      checkmate::assert_string(underlying_id)
      checkmate::assert_number(delta)
      checkmate::assert_string(instrument_type)
      checkmate::assert_string(description)
      checkmate::assert_list(fields, names = "unique")
      if (is.null(derive_price)) {
        derive_price <- function(underlying_price, delta) {
          abs(delta) * underlying_price
        }
      }
      checkmate::assert_function(derive_price)
      private$overrides_[[tolower(sec_id)]] <- list(
        description = description,
        instrument_type = instrument_type,
        delta = delta,
        underlying_id = tolower(underlying_id),
        derive_price = derive_price,
        fields = fields
      )
      invisible(self)
    },
    #' @description Registered override ids (lowercase).
    override_ids = function() names(private$overrides_),
    #' @description Whether a security is served as an override.
    #' @param sec_id Character. Security identifier.
    is_registered = function(sec_id) private$is_override(sec_id),

    # SecurityDataProvider interface -----------------------------------------
    #' @description Check existence (override or primary).
    #' @param sec_id Character. Security identifier.
    security_exists = function(sec_id) {
      if (private$is_override(sec_id)) return(TRUE)
      private$primary_$security_exists(sec_id)
    },
    #' @description Description (override or primary).
    #' @param sec_id Character. Security identifier.
    get_description = function(sec_id) {
      ov <- private$get_override(sec_id)
      if (!is.null(ov)) return(ov$description)
      private$primary_$get_description(sec_id)
    },
    #' @description Instrument type (override or primary).
    #' @param sec_id Character. Security identifier.
    get_instrument_type = function(sec_id) {
      ov <- private$get_override(sec_id)
      if (!is.null(ov)) return(ov$instrument_type)
      private$primary_$get_instrument_type(sec_id)
    },
    #' @description Underlying id (override or primary).
    #' @param sec_id Character. Security identifier.
    get_underlying_id = function(sec_id) {
      ov <- private$get_override(sec_id)
      if (!is.null(ov)) return(ov$underlying_id)
      private$primary_$get_underlying_id(sec_id)
    },
    #' @description Prices for a vector of securities: overrides derived from
    #'  their underlying's primary price, the rest served by \code{primary}.
    #' @param sec_ids Character vector of security identifiers.
    get_prices = function(sec_ids) {
      out <- setNames(rep(NA_real_, length(sec_ids)), sec_ids)
      is_ov <- vapply(sec_ids, private$is_override, logical(1))
      if (any(!is_ov)) {
        prim <- private$primary_$get_prices(sec_ids[!is_ov])
        out[!is_ov] <- prim[sec_ids[!is_ov]]
      }
      for (id in sec_ids[is_ov]) {
        out[[id]] <- private$derive_override_price(private$get_override(id))
      }
      out
    },
    #' @description Deltas for a vector of securities: signed override delta or
    #'  \code{primary}.
    #' @param sec_ids Character vector of security identifiers.
    get_deltas = function(sec_ids) {
      out <- setNames(rep(NA_real_, length(sec_ids)), sec_ids)
      is_ov <- vapply(sec_ids, private$is_override, logical(1))
      if (any(!is_ov)) {
        prim <- private$primary_$get_deltas(sec_ids[!is_ov])
        out[!is_ov] <- prim[sec_ids[!is_ov]]
      }
      for (id in sec_ids[is_ov]) {
        out[[id]] <- private$get_override(id)$delta
      }
      out
    },
    #' @description Arbitrary fields for a vector of securities. Override rows
    #'  are built locally (PX_LAST = derived price, OP006 = signed delta, other
    #'  fields = registered value or NA); the rest come from one
    #'  \code{primary$get_fields()} call. Rows returned in \code{sec_ids} order.
    #' @param sec_ids Character vector of security identifiers.
    #' @param fields Character vector of field mnemonics.
    get_fields = function(sec_ids, fields) {
      is_ov <- vapply(sec_ids, private$is_override, logical(1))
      prim_ids <- sec_ids[!is_ov]
      ov_ids <- sec_ids[is_ov]

      prim_df <- if (length(prim_ids)) {
        private$primary_$get_fields(prim_ids, fields)[, fields, drop = FALSE]
      }
      ov_df <- if (length(ov_ids)) {
        cols <- lapply(fields, function(f) {
          vapply(ov_ids, function(id) {
            ov <- private$get_override(id)
            val <- if (identical(f, "PX_LAST")) {
              private$derive_override_price(ov)
            } else if (identical(f, "OP006")) {
              ov$delta
            } else {
              ov$fields[[f]] %||% NA
            }
            as.numeric(val)
          }, numeric(1))
        })
        d <- data.frame(cols, stringsAsFactors = FALSE)
        colnames(d) <- fields
        rownames(d) <- ov_ids
        d
      }

      combined <- if (!is.null(prim_df) && !is.null(ov_df)) {
        rbind(prim_df, ov_df)
      } else if (!is.null(ov_df)) {
        ov_df
      } else {
        prim_df
      }
      combined[sec_ids, , drop = FALSE]
    },
    #' @description Full profile for a single security.
    #' @param sec_id Character. Security identifier.
    get_security_profile = function(sec_id) {
      ov <- private$get_override(sec_id)
      if (is.null(ov)) {
        return(private$primary_$get_security_profile(sec_id))
      }
      list(
        description     = ov$description,
        instrument_type = ov$instrument_type,
        price           = private$derive_override_price(ov),
        delta           = ov$delta,
        underlying_id   = ov$underlying_id
      )
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
