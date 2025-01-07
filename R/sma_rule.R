# Object Definition ------------------------------------------------------------
#' @title SMA Rule (S4 Object)
#' @description Parent Class for SMA Rules
#' @slot rule_id Rule ID
#' @slot rule_name Rule Name
#' @slot rule_scope Rule Scope (Agg/SMA)
#' @slot rule_type Rule Type
setClass(
  "sma_rule",
  representation(
    rule_id = "character",
    rule_name = "character",
    rule_scope = "character",
    rule_type = "character"
  )
)