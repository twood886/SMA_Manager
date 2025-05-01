#' @title Trade (R6 Object)
#' @description
#' R6 Class representing a trade object.
#' 
#' @import R6
Trade <- R6::R6Class(  #nolint
  "Trade",
  public = list(
    #' @description
    #' Create New Trade R6 object
    #' @param security_id Security id
    initialize = function(
      security_id = NULL
    ) {
      private$id_ <- length(ls(envir = .portfolio_registry)) + 1
      private$security_id_ <- security_id
      private$total_qty_ <- 0
      private$alloction_shares_ <- list()
      private$allocation_pct_ <- list()
    },

    # Getter Functions ---------------------------------------------------------
    #' @description Get trade id
    get_id = function() private$id_,
    #' @description Get trade portfolio
    get_portfolio = function() private$portfolio_,
    #' @description Get trade position
    get_position = function() private$position_,

    #' @description Add Trade to Trade Object
    #' @param portfolio_short_name Portfolio Short Name
    #' @param quantity Quantity of Trade
    #' @return Null
    add_trade = function(
      portfolio_short_name = NULL,
      quantity = NULL
    ) {
      if (is.null(portfolio_short_name) || is.null(quantity)) {
        stop("Portfolio short name and quantity must be provided.")
      }
      if (!exists(short_name, envir = .portfolio_registry, inherits = FALSE)) {
        stop("Specified portfolio has not been created")
      }
      private$alloction_shares_[[portfolio_short_name]] <- quantity
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
      for (portfolio_short_name in names(private$alloction_shares_)) {
        shares <- private$alloction_shares_[[portfolio_short_name]]
        pct <- shares / private$total_qty_
        private$allocation_pct_[[portfolio_short_name]] <- pct
      }
      invisible(NULL)
    },
    #' @description Output to data.frame
    #' @return Data frame of trade allocation
    to_df = function() {
      data.frame(
        portfolio_short_name = names(private$alloction_shares_),
        shares = unlist(private$alloction_shares_),
        allocation_pct = unlist(private$allocation_pct_)
      )
    }
  ),
  private = list(
    trade_id_ = NULL,
    security_id_ = NULL,
    total_qty_ = NULL,
    allocation_shares_ = NULL,
    allocation_pct_ = NULL
  )
)