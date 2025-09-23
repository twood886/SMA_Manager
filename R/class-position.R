#' @title Position (R6 Object)
#' @description
#' R6 Class representing a position object.
#' A position object contains information about the size of the
#'  position (in relation to a portfolio or SMA object) as well as
#'  underlying security R6 object.
#' @import R6
#' @include utils.R
#' @include api-functions.R
#' @include class-security.R
#' @include class-holding.R
#' @export
Position <- R6::R6Class(  #nolint
  "Position",
  private = list(
    portfolio_short_name_ = NULL,
    id_ = NULL,
    security_ = NULL,
    holdings_ = list()
  ),
  public = list(
    #' @description
    #' Create New PositionR6 object
    #' @param portfolio_short_name Portfolio_Short_name
    #' @param security Security
    initialize = function(portfolio_short_name, security) {
      checkmate::assert_character(portfolio_short_name)
      checkmate::assert_r6(security, "Security")
      private$portfolio_short_name_ <- portfolio_short_name
      private$security_ <- security
      private$id_ <- security$get_id()
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get position ticker
    get_id = function() private$id_,
    #' @description Get position security
    get_security = function() private$security_,
    #' @description Get position Quantity
    get_qty = function() {
      sum(vapply(private$holdings_, function(x) x$get_qty(), numeric(1)))
    },
    #' @description Get Swap flag
    get_swap = function() {
      any(vapply(private$holdings_, function(x) x$get_swap(), logical(1)))
    },
    #' @description Get position Delta Quantity
    get_delta_qty = function() {
      self$get_qty() * self$get_security()$get_delta()
    },
    #' @description Get position Market Value
    get_mkt_val = function() {
      self$get_qty() * self$get_security()$get_price()
    },
    #' @description Get position Delta Value
    get_delta_val = function() {
      self$get_delta_qty() * self$get_security()$get_price()
    },
    #' @description Get position Stock Percent of NAV
    #' @param nav Portfolio NAV
    get_mkt_pct_nav = function(nav = NULL) {
      tryCatch(
        {
          checkmate::assert_numeric(nav)
          self$get_mkt_val() / nav
        },
        error = function(e) {
          port <- 
          nav <- port$get_nav()
          self$get_mkt_val() / nav
        }
      )
    },
    #' @description Get position Delta Percent of NAV
    #' @param nav Portfolio NAV
    get_delta_pct_nav = function(nav = NULL) {
      tryCatch(
        {
          checkmate::assert_numeric(nav)
          self$get_delta_val() / nav
        },
        error = function(e) {
          port <- .portfolio(private$portfolio_short_name_, create = FALSE)
          nav <- port$get_nav()
          self$get_delta_val() / nav
        }
      )
    },
    #' @description Get all holdings associated with position
    get_holdings = function() private$holdings_,
    # Setter Functions ---------------------------------------------------------
    #' @description Add Holding to Position
    #' @param holding Holding R6 object
    add_holding = function(holding) {
      checkmate::assert_r6(holding, "Holding")
      if (holding$get_security_id() != self$get_id()) {
        stop("Holding ID does not match Position ID")
      }
      private$holdings_[[length(private$holdings_) + 1]] <- holding
    }
  )
)
