#' @title SMA Rule for Portfolio
#' @import R6
#' @include class-smarule.R
#' @export
SMARulePortfolio <- R6::R6Class( #nolint
  "SMARulePortfolio",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against the current portfolio
    check_rule_current = function() {
      private$check_rule_position_(private$get_sma_()$get_position())
    },
    #' @description Check the rule against the target portfolio
    check_rule_target = function() {
      private$check_rule_position_(private$get_sma_()$get_target_position())
    },
    #' @description Get the Max and Min value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
    }
  ),
  private = list(
    get_sma_ = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = TRUE)
    },
    check_rule_position_ = function(positions) {
      v <- sum(private$apply_rule_definition(positons), na.rm = TRUE)
      comply <- v <= private$max_threshold_ && v >= private$min_threshold_
      result <- list("pass" = comply)
      result
    },
    apply_rule_definition = function(positions) {
      private$definition_(positions, private$get_sma_())
    }
  )
)





.sma_rule(
  sma_name = "fmap",
  rule_name = "Maxumum Gross Exposure",
  scope = "portfolio",
  definition = function(positions, portfolio) {
    delta_val <- vapply(positions, \(pos) abs(pos$get_delta_val()), numeric(1))
    nav <- portfolio$get_nav()
    gross_exp <- delta_val / nav
    setNames(gross_exp, sapply(positions, \(pos) pos$get_id()))
  },
  swap_only = FALSE,
  max_threshold = 2,
  min_threshold = -Inf
)