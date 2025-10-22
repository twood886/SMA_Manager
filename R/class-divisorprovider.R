#' @title DivisorProvider Class
#'
#' @description R6 class to provide divisor values for weight normalization in
#'  optimization. Supports several common types: NAV (no adjustment),
#'  GMV (gross market value), long GMV, and short GMV.
#'
#' @details This class is used by the TradeConstructor to normalize weights
#'  during optimization. The divisor can be a fixed numeric value
#'  (e.g., 1 for NAV normalization) or a CVXR expression that captures
#'  the desired normalization metric (e.g., GMV).
DivisorProvider <- R6::R6Class( #nolint
  "DivisorProvider",
  public = list(
    #' @field kind (`character(1)`)\cr
    #' Indicates the type of divisor: "nav", "gmv", "long_gmv", or "short_gmv"
    kind = NULL,

    #' @description Initialize a DivisorProvider
    #' @param kind Character string specifying the type of divisor:
    #'  "nav" (default), "gmv", "long_gmv", or "short_gmv"
    initialize = function(kind = "nav") self$kind <- kind,

    #' Compute Weights for a given portfolio and set of securities
    #' @param portfolio Portfolio object
    #' @param ids Character vector of security IDs
    #' @param shares Optional numeric vector of shares corresponding to ids.
    #'  If NULL, current positions in the portfolio are used.
    weights = function(portfolio, ids, shares = NULL) {
      nav <- portfolio$get_nav()
      if (is.null(ids)) {
        pos <- portfolio$get_position()
        ids <- vapply(pos, \(p) p$get_id(), character(1))
        qty <- vapply(pos, \(p) p$get_qty(), numeric(1))
      } else {
        if (is.null(shares)) {
          pos <- portfolio$get_position()
          pid <- vapply(pos, \(p) p$get_id(), character(1))
          qty <- vapply(pos, \(p) p$get_qty(), numeric(1))
          m <- match(pid, ids)
          qty <- ifelse(is.na(m), 0, qty[m])
        } else {
          qty <- shares
        }
      }

      price <- vapply(ids, \(id) .security(id)$get_price(), numeric(1))
      price[!is.finite(price) | price <= 0] <- 1
      qty * price / nav
    },

    #' @description Per-name contribution to the divisor (vector)
    #' @param w Numeric vector of weights
    contrib_vec = function(w) {
      switch(self$kind,
        "nav"       = rep(1, length(w)),
        "gmv"       = abs(w),
        "long_gmv"  = pmax(w, 0),
        "short_gmv" = pmax(-w, 0),
        stop("Unknown divisor kind: ", self$kind)
      )
    },


    #' @description Get numeric divisor value for checks or UI
    #' @param portfolio Portfolio object
    #' @param ids Character vector of security IDs
    #' @param shares Optional numeric vector of shares corresponding to ids.
    value = function(portfolio, ids, shares = NULL) {
      if (self$kind == "nav") return(1)
      w <- self$weights(portfolio, ids, shares)
      sum(self$contrib_vec(w))
    },


    #' @description Get CVXR expression for divisor
    #' @param ctx Context object with metric helper
    expr = function(ctx) {
      # leverage your ctx$metric helper if you added it
      if (self$kind == "nav") {
        return(list(expr = 1, cons = list()))
      }
      if (self$kind == "gmv") {
        m <- ctx$metric("gmv")
        return(list(expr = m$var, cons = m$cons))
      }
      if (self$kind == "long_gmv") {
        m <- ctx$metric("long_gmv")
        return(list(expr = m$var, cons = m$cons))
      }
      if (self$kind == "short_gmv") {
        m <- ctx$metric("short_gmv")
        return(list(expr = m$var, cons = m$cons))
      }
      stop("Unknown divisor kind: ", self$kind)
    }
  )
)
