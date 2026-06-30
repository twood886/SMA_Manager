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
    sec_id_ = NULL,
    qty_ = NULL,
    swap_ = NULL,
    custodian_acct_id_ = NULL,
    trs_custodian_id_ = NULL
  ),
  public = list(
    #' @description
    #' Create New Holding R6 object
    #' @param sec_id Security ID (Ticker)
    #' @param qty Stock Quantity
    #' @param swap Swap Flag
    #' @param custodian_acct_id Custodian Account ID
    #' @param trs_custodian_id TRS Custodian ID
    initialize = function(
      sec_id, qty, swap = FALSE,
      custodian_acct_id = NULL, trs_custodian_id = NULL
    ) {
      checkmate::assert_character(sec_id)
      checkmate::assert_numeric(qty)
      checkmate::assert_flag(swap)
      checkmate::assert_character(custodian_acct_id, null.ok = TRUE)
      checkmate::assert_character(trs_custodian_id, null.ok = TRUE)
      private$id_ <- paste(sec_id, custodian_acct_id, sep = "|")
      private$sec_id_ <- sec_id
      private$qty_ <- qty
      private$swap_ <- swap
      private$custodian_acct_id_ <- custodian_acct_id
      private$trs_custodian_id_ <- trs_custodian_id
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get holding ticker
    get_id = function() private$id_,
    #' @description Get holding Quantity
    get_qty = function() private$qty_,
    #' @description Get Swap flag
    get_swap = function() private$swap_,
    #' @description Get Custodian Account ID
    get_custodian_acct_id = function() private$custodian_acct_id_,
    #' @description Get TRS Custodian ID
    get_trs_custodian_id = function() private$trs_custodian_id_,
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
