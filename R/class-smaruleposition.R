#' @title SMA Rule for Positions
#'
#' @description R6 Class that encapsulates rule for SMAs
#'
#' @import R6
#'
#' @export
SMARulePosition <- R6::R6Class( #nolint
  "SMARulePosition",
  inherit = SMARule,
  public = list(
    #' Get SMA
    #' @description Get SMA the Rule applies to
    get_sma = function() {
      sma <- get(private$sma_name_, envir = .sma_registry, inherits = FALSE)
      return(sma)
    },
    #' Check Rule Against Current Holdings
    #' @description Check the rule against the current holdings
    check_rule_current = function() {
      positions <- private$get_positions_current_()
      check <- sapply(
        lapply(positions, private$definition_),
        private$threshold_
      )
      breaks <- which(!check)
      return(sapply(positions[breaks], function(x) x$get_id()))
    }
  ),
  private = list(
    get_positions_current_ = function() {
      sma <- self$get_sma()
      positions <- sma$get_positions()
    }
  )
)
