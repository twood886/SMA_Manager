OrderConstructor <- R6::R6Class(
  "OrderConstructor",
  private = list(
    pb_act_num_ = NULL,
    pb_act_sel_ = NULL,
    isda_act_sel_ = NULL
  ),
  public = list(
    #' Create New OrderConstructor R6 Object
    #' @param pb_act_num List containing the account number for PB and ISDA
    #' accounts by Broker
    #' @param pb_act_sel function taking security_id and returning the PB
    #' account number to use to trade when creating a new position
    #' @param isda_act_sel function taking security_id and returning the ISDA
    #' account number to use to trade when creating a new position
    initialize = function(pb_act_num, pb_act_sel, isda_act_sel) {
      private$pb_act_num_ <- pb_act_num
      private$pb_act_sel_ <- pb_act_sel
      private$isda_act_sel_ <- isda_act_sel
    },

    #' Get holdings for a specified security from a portfolio
    #' @param portfolio An R6 Portfolio object
    #' @param security_id security id
    #' @return a list of holdings in the security id from the portfolio. Returns
    #'  NULL if there are no holdings for the supplied security in the supplied
    #'  portfolio.
    get_holdings = function(portfolio, security_id) {
      checkmate::assert_r6(portfolio, "Portfolio")
      checkmate::assert_character(security_id)
      position <- tryCatch(
        portfolio$get_position(security_id),
        error = function(e) NULL
      )
      if (is.null(position)) return(NULL)
      position$get_holdings()
    },

    #' Get the quantities from a list of holdings
    #' @param holdings A list of Holding R6 objects
    #' @return a vector containing the quantities for each of the holdings, or
    #'  NULL is there are no holdings
    get_qty_from_holdings = function(holdings) {
      if (is.null(holdings)) return(NULL)
      checkmate::assert_list(holdings, "Holding")
      vapply(holdings, \(h) h$get_qty(), numeric(1))
    },

    #' Sorts a list of holdings based on the quantity held.
    #' @param holdings A list of Holding R6 objects
    #' @param decreasing Logical, whether the list of holdings should be sorted
    #'  in decreasing order. Default FALSE
    #' @return A list of holdings, sorted by the quantity held
    sort_holdings_qty = function(holdings, decreasing = FALSE) {
      if (is.null(holdings)) return(NULL)
      checkmate::assert_list(holdings, "Holding")
      checkmate::assert_logical(decreasing)
      qtys <- vapply(holdings, \(h) h$get_qty(), numeric(1))
      qtys_ord <- order(qtys, decreasing = decreasing)
      holdings[qtys_ord]
    },

    #' Get Long Holdings Sorted by Quantity Held
    #' @param holdings A list of Holding R6 objects
    #' @param decreasing Logical, whether the list of holdings should be sorted
    #'  in decreasing order. Default FALSE
    #' @return A list of holdings where the quantity is greater than 0, sorted
    #'  by the quasntity held
    get_long_holdings = function(holdings, decreasing = FALSE) {
      qtys <- self$get_qty_from_holdings(holdings)
      if (is.null(qtys)) return(NULL)
      self$sort_holdings_qty(holdings[which(qtys > 0)], decreasing = decreasing)
    },

    #' Get Short Holdings Sorted by Quantity Held
    #' @param holdings A list of Holding R6 objects
    #' @param decreasing Logical, whether the list of holdings should be sorted
    #'  in decreasing order. Default FALSE
    #' @return A list of holdings where the quantity is less than 0, sorted
    #'  by the quasntity held
    get_short_holdings = function(holdings, decreasing = FALSE) {
      qtys <- self$get_qty_from_holdings(holdings)
      if (is.null(qtys)) return(NULL)
      self$sort_holdings_qty(holdings[which(qtys < 0)], decreasing = decreasing)
    },

    #' Get list of trades for Selling
    #' @param security_id securtiy_id
    #' @param holdings A list of Holdings R6 objects
    #' @param shares Number of shares to trade
    #' @param swap Logical, is trade to be do on swap
    sell = function(security_id, holdings, shares, swap) {
      checkmate::assert(
        checkmate::check_list(holdings, "Holding"),
        checkmate::check_null(holdings)
      )
      checkmate::assert_number(shares)
      checkmate::assert_logical(swap)
      if (shares > 0) stop("Shares Expected to be Negative")
      long_holdings <- self$get_long_holdings(holdings, decreasing = FALSE)
      short_holdings <- self$get_short_holdings(holdings, decreasing = FALSE)
      trades <- c()
      remaining_to_sell <- shares

      # Sell Long Holdings First
      if (!is.null(long_holdings) && remaining_to_sell < 0) {
        long_qtys <- self$get_qty_from_holdings(long_holdings)
        for (i in seq_along(long_holdings)) {
          if (remaining_to_sell == 0) break
          h_obj <- long_holdings[[i]]
          qty_avail <- long_qtys[i]
          trade_amt <- max(-qty_avail, remaining_to_sell)
          new_trade <- self$create_trade_existing(h_obj, trade_amt)
          trades <- append(trades, list(new_trade))
          remaining_to_sell <- remaining_to_sell - trade_amt
        }
      }
      if (remaining_to_sell == 0) return(trades)

      # Add To Existing Holdings
      if (!is.null(short_holdings) && length(short_holdings) > 0) {
        top_h <- short_holdings[[1]]
        add_to_short <- self$create_trade_existing(top_h, remaining_to_sell)
        trades <- append(trades, list(add_to_short))
        return(trades)
      }

      # Create New Trades
      new_trade <- self$create_trade_new(security_id, remaining_to_sell, swap)
      trades <- append(trades, list(new_trade))
      trades
    },

    #' Get list of trades for Buying
    #' @param security_id security id
    #' @param holdings A list of Holdings R6 objects
    #' @param shares Number of shares to trade
    #' @param swap Logical, is trade to be do on swap
    buy = function(security_id, holdings, shares, swap) {
      checkmate::assert(
        checkmate::check_list(holdings, "Holding"),
        checkmate::check_null(holdings)
      )
      checkmate::assert_number(shares)
      checkmate::assert_logical(swap)
      if (shares < 0) stop("Shares Expected to be Positive")
      long_holdings <- self$get_long_holdings(holdings, decreasing = TRUE)
      short_holdings <- self$get_short_holdings(holdings, decreasing = TRUE)
      trades <- c()
      remaining_to_buy <- shares

      # Buy Shorted Holdings First
      if (!is.null(short_holdings) && remaining_to_buy > 0) {
        short_qtys <- self$get_qty_from_holdings(short_holdings)
        for (i in seq_along(short_holdings)) {
          if (remaining_to_buy == 0) break
          h_obj <- short_holdings[[i]]
          qty_avail <- short_qtys[i]
          trade_amt <- min(-qty_avail, remaining_to_buy)
          new_trade <- self$create_trade_existing(h_obj, trade_amt)
          trades <- append(trades, list(new_trade))
          remaining_to_buy <- remaining_to_buy - trade_amt
        }
      }
      if (remaining_to_buy == 0) return(trades)

      # Add to existing Long Holdings
      if (!is.null(long_holdings) && length(long_holdings) > 0) {
        top_h <- long_holdings[[1]]
        add_to_long <- self$create_trade_existing(top_h, remaining_to_buy)
        trades <- append(trades, list(add_to_long))
        return(trades)
      }

      # Create new trade
      new_trade <- self$create_trade_new(security_id, remaining_to_buy, swap)
      trades <- append(trades, list(new_trade))
      trades
    },

    #' Make Trade
    #' @param portfolio R6 Portfolio Object
    #' @param security_id Security Id
    #' @param shares Number of shares to trade
    #' @param swap Logical, swap
    trade = function(portfolio, security_id, shares, swap) {
      checkmate::assert_r6(portfolio, "Portfolio")
      checkmate::assert_character(security_id)
      checkmate::assert_numeric(shares)
      checkmate::assert_logical(swap)

      holdings <- self$get_holdings(portfolio, security_id)
      if (shares > 0) {
        return(self$buy(security_id, holdings, shares, swap))
      }

      if (shares < 0) {
        return(self$sell(security_id, holdings, shares, swap))
      }
      stop("Error")
    },

    #' Create Trade Existing
    #' @param holding An R6 Holding object
    #' @param qty Number of shares to trade in holding
    create_trade_existing = function(holding, qty) {
      trade <- list(
        "id" = holding$get_security_id(),
        "broker_id" = holdings$get_custodian_act_id(),
        "qty" = qty
      )
    },

    #' Create a Trade where no existing holding exists
    #' @param security_id security id
    #' @param qty Number of shares to trade in holding
    #' @param swap Logical, swap
    create_trade_new = function(security_id, qty, swap) {
      if (isTRUE(swap)) {
        acct <- private$isda_act_sel_(security_id)
      } else {
        acct <- private$pb_act_sel_(security_id)
      }
      trade <- list(
        "id" = holding$get_security_id(),
        "broker_id" = acct,
        "qty" = qty
      )
    }
  )
)