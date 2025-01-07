#' @title Create SMA Rule
#' @description Create an SMA Rule
#' @param name Rule name
#' @param scope Rule scope
#' @param desc Rule description
#' @param func Rule function
create_sma_rule <- function(
  name, scope = c("agg", "ind"), desc, func
) {

  if (!scope %in% c("agg", "ind")) {
    stop("rule_scope must be 'agg' or 'ind'")
  }

  if (scope == "ind") {
    new(
      "sma_rule_port",
      rule_name = name,
      rule_scope = scope,
      rule_desc = desc,
      position_func = func
    )
  }
}