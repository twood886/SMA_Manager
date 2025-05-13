#' @title Trade (R6 Object)
#' @description
#' R6 Class representing a trade object.
#' 
#' @import R6
#' @export
Trade <- R6::R6Class(  #nolint
  "Trade",
  private = list(
    trade_id_ = NULL,
    security_id_ = NULL,
    total_qty_ = NULL,
    allocation_shares_ = NULL,
    allocation_pct_ = NULL,
    swap_ = NULL
  ),
  public = list(
    #' @description
    #' Create New Trade R6 object
    #' @param security_id Security id
    #' @param swap Swap flag
    initialize = function(security_id = NULL, swap = FALSE) {
      private$trade_id_ <- length(ls(envir = registries$trades)) + 1
      private$security_id_ <- security_id
      private$total_qty_ <- 0
      private$allocation_shares_ <- list()
      private$allocation_pct_ <- list()
      private$swap_ <- swap
    },

    # Getter Functions ---------------------------------------------------------
    #' @description Get trade id
    get_id = function() private$trade_id_,
    #' @description Get security id
    get_security_id = function() private$security_id_,
    #' @description Get total quantity
    get_total_qty = function() private$total_qty_,
    #' @description Get allocation shares
    get_allocation_shares = function() private$allocation_shares_,
    #' @description Get allocation percentage
    get_allocation_pct = function() private$allocation_pct_,
    #' @description Get swap flag
    get_swap_flag = function() private$swap_,
    #' @description Get trade quantity allocation
    #' @param portfolio_short_name Portfolio Short Name
    get_trade_allocation_qty = function(portfolio_short_name = NULL) {
      if (is.null(portfolio_short_name)) {
        stop("Portfolio short name must be provided.")
      }
      if (!exists(portfolio_short_name, envir = registries$portfolios, inherits = FALSE)) { #nolint
        stop("Specified portfolio has not been created")
      }
      if (!portfolio_short_name %in% names(private$allocation_shares_)) {
        stop("Portfolio short name not found in allocation shares.")
      }
      private$allocation_shares_[[portfolio_short_name]]
    },
    #' @description Add Trade to Trade Object
    #' @param portfolio_short_name Portfolio Short Name
    #' @param quantity Quantity of Trade
    #' @return Null
    add_trade_qty = function(portfolio_short_name = NULL, quantity = NULL) {
      if (is.null(portfolio_short_name) || is.null(quantity)) {
        stop("Portfolio short name and quantity must be provided.")
      }
      if (!exists(portfolio_short_name, envir = registries$portfolios, inherits = FALSE)) { #nolint
        stop("Specified portfolio has not been created")
      }

      existing_trade_qty <- private$allocation_shares_[[portfolio_short_name]]
      if (is.null(existing_trade_qty)) {
        existing_trade_qty <- 0
      }

      private$allocation_shares_[[portfolio_short_name]] <- quantity + existing_trade_qty
      private$total_qty_ <- private$total_qty_ + quantity
      self$allocate_trade()
      invisible(NULL)
    },
    #' @description Calculate Trade Allocation
    #' @return Null
    allocate_trade = function() {
      if (private$total_qty_ == 0) {
        stop("Total quantity is zero, cannot allocate trade.")
      }
      for (portfolio_short_name in names(private$allocation_shares_)) {
        shares <- private$allocation_shares_[[portfolio_short_name]]
        pct <- shares / private$total_qty_
        private$allocation_pct_[[portfolio_short_name]] <- pct
      }
      invisible(NULL)
    },
    #' @description Remove Trade from Trade Object
    #' @param portfolio_short_name Portfolio Short Name
    #' @param qty Quantity of Trade to remove
    remove_trade_qty = function(portfolio_short_name = NULL, qty = NULL) {
      if (is.null(portfolio_short_name)) {
        stop("Portfolio short name must be provided.")
      }
      if (!exists(portfolio_short_name, envir = registries$portfolios, inherits = FALSE)) { #nolint
        stop("Specified portfolio has not been created")
      }
      if (!portfolio_short_name %in% names(private$allocation_shares_)) {
        return(invisible(NULL))
      }
      if (is.null(qty)) {
        qty <- -self$get_trade_allocation_qty(portfolio_short_name)
      }
      self$add_trade_qty(portfolio_short_name, qty)
      invisible(NULL)
    },
    #' @description Output to data.frame
    #' @return Data frame of trade allocation
    to_df = function() {
      data.frame(
        "portfolio_short_name" = names(private$allocation_shares_),
        "shares" = unlist(private$allocation_shares_),
        "allocation_pct" = unlist(private$allocation_pct_),
        "swap" = private$swap_,
        "security_id" = private$security_id_
      )
    }
  )
)
