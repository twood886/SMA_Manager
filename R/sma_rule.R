#' @title SMA Rule (S4 Object)
#' @description Parent Class for SMA Rules
#' @slot rule_id Rule ID
#' @slot rule_name Rule Name
#' @slot rule_scope Rule Scope (Agg/SMA)
#' @slot rule_desc Rule Type
setClass(
  "sma_rule",
  representation(
    rule_id = "character",
    rule_name = "character",
    rule_scope = "character",
    rule_desc = "character"
  )
)

#' @title SMA Position Rule (S4 Object)
#' @description SMA Rule about positions in SMA
#' @slot position_func Function to be applied to functions
setClass("sma_rule_port",
  contains = "sma_rule",
  representation(
    position_func = "function"
  )
)