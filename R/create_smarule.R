#' @title Create SMA Rule
#' @description
#' Function to create a SMA Rule object
#' @param sma_name Name of SMA the rule applies to
#' @param rule_name Name of the SMA rule
#' @param scope Scope of the rule (position)
#' @param definition Function to define the rule
#' @param threshold Function to define the threshold
#' @return NULL
#' @export
create_smarule <- function(
  sma_name = NULL,
  rule_name = NULL,
  scope = NULL,
  definition = NULL,
  max_threshold = Inf,
  min_threshold = -Inf
) {
  if (scope == "position") {
    smarule <- SMARulePosition$new(
      sma_name = sma_name,
      name = rule_name,
      scope = scope,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold
    )
  }

  sma <- get_portfolio(sma_name)
  sma$add_rule(smarule)
  # Add the rule to the registry
  assign(
    paste(sma_name, smarule$get_id()),
    smarule,
    envir = .smarule_registry
  )
  invisible(NULL)
}
