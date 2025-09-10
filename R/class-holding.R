#' @title Holding
#' @description
#' R6 Class representing a holding object.
#' @import R6
#' @import checkmate
#' @export
Holding <- R6::R6Class( 
  "Holding",
  private = list(
    id_ = NULL,
    portfolio_short_name_ = NULL,
    sec_id_ = NULL,
    qty_ = NULL,
    swap_ = NULL,
    custodian_ = NULL,
    broker_custodian_ = NULL,
    custodian_acct_id_ = NULL,
    custodian_acct_ = NULL,
    trs_custodian_id_ = NULL,
    trs_custodian_name_ = NULL
  ),
  public = list(
    #' @description
    #' Create New Holding R6 object
    #' @param portfolio_short_name Portfolio_Short_name
    #' @param sec_id Security ID (Ticker)
    #' @param qty Stock Quantity
    #' @param swap Swap Flag
    #' @param custodian Custodian Name
    #' @param broker_custodian Broker Custodian Name
    #' @param custodian_acct_id Custodian Account ID
    #' @param custodian_acct Custodian Account Name
    #' @param trs_custodian_id TRS Custodian ID
    #' @param trs_custodian_name TRS Custodian Name
    initialize = function(
      portfolio_short_name, sec_id, qty, swap = FALSE,
      custodian = NULL, broker_custodian = NULL,
      custodian_acct_id = NULL, custodian_acct = NULL,
      trs_custodian_id = NULL, trs_custodian_name = NULL
    ) {
      checkmate::assert_character(sec_id)
      checkmate::assert_numeric(qty)
      checkmate::assert_flag(swap)
      checkmate::assert_character(custodian, null.ok = TRUE)
      checkmate::assert_character(broker_custodian, null.ok = TRUE)
      checkmate::assert_character(custodian_acct_id, null.ok = TRUE)
      checkmate::assert_character(custodian_acct, null.ok = TRUE)
      checkmate::assert_character(trs_custodian_id, null.ok = TRUE)
      checkmate::assert_character(trs_custodian_name, null.ok = TRUE)
      private$portfolio_short_name_ <- portfolio_short_name
      private$id_ <- paste(sec_id, custodian_acct_id, sep = "|")
      private$sec_id_ <- sec_id
      private$qty_ <- qty
      private$swap_ <- swap
      private$custodian_ <- custodian
      private$broker_custodian_ <- broker_custodian
      private$custodian_acct_id_ <- custodian_acct_id
      private$custodian_acct_ <- custodian_acct
      private$trs_custodian_id_ <- trs_custodian_id
      private$trs_custodian_name_ <- trs_custodian_name
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get holding ticker
    get_id = function() private$id_,
    #' @description Get holding Quantity
    get_qty = function() private$qty_,
    #' @description Get Swap flag
    get_swap = function() private$swap_,
    #' @description Get Custodian Name
    get_custodian = function() private$custodian_,
    #' @description Get Broker Custodian Name
    get_broker_custodian = function() private$broker_custodian_,
    #' @description Get Custodian Account ID
    get_custodian_acct_id = function() private$custodian_acct_id_,
    #' @description Get Custodian Account Name
    get_custodian_acct = function() private$custodian_acct_,
    #' @description Get TRS Custodian ID
    get_trs_custodian_id = function() private$trs_custodian_id_,
    #' @description Get TRS Custodian Name
    get_trs_custodian_name = function() private$trs_custodian_name_,
    #' @description Get Security ID
    get_security_id = function() private$sec_id_,

    # Setter Functions ---------------------------------------------------------
    #' @description Set Qty
    #' @param qty New Quantity
    set_qty = function(qty) {
      checkmate::assert_numeric(qty)
      private$qty_ <- qty
      invisible(NULL)
    }
  )
)
