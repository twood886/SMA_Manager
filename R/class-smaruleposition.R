#' @title SMA Rule for Positions
#'
#' @description R6 Class that encapsulates rule for SMAs
#'
#' @import R6
#' @include class-smarule.R
#'
#' @export
SMARulePosition <- R6::R6Class( #nolint
  "SMARulePosition",
  inherit = SMARule,
  public = list(
    #' Get SMA
    #' @description Get SMA the Rule applies to
    get_sma = function() {
      get(private$sma_name_, envir = .portfolio_registry, inherits = FALSE)
    },
    #' Check Rule Against Current Holdings
    #' @description Check the rule against the current holdings
    check_rule_current = function() {
      positions <- self$get_sma()$get_position()
      check <- sapply(
        lapply(positions, private$definition_),
        private$threshold_
      )
      breaks <- which(!check)
      sapply(positions[breaks], function(x) x$get_id())
    },
    #' Check Rule Against Target Holdings
    #' @description Check the rule against the target holdings
    check_rule_target = function() {
      positions <- self$get_sma()$get_target_position()
      check <- sapply(
        lapply(positions, private$definition_),
        private$threshold_
      )
      breaks <- which(!check)
      sapply(positions[breaks], function(x) x$get_id())
    }
  )
)
