# SMAConstrucor

SMAConstructor <- R6::R6Class( #nolint
  "SMAConstructor",
  public(
    initialize = function(),

    get_security_position_limits = function(security_id = NULL, rules) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      non_swap_rules <- rules[
        !vapply(rules, \(rule) rule$get_swap_only(), logical(1))
      ]
      limits <- lapply(
        non_swap_rules,
        \(rule) rule$get_security_limits(security_id)
      )
      max_limits <- sapply(limits, \(x) x$max)
      min_limits <- sapply(limits, \(x) x$min)
      list(max_shares = min(max_limits), min_shares = max(min_limits))
    },



    get_swap_flag_position_rules = function(security_id = NULL, rules) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      any(vapply(
        private$sma_rules_,
        \(rule) rule$check_swap_security(security_id),
        logical(1)
      ))
    },


    rebalance_position = function(
      security_id,
      base_target_position,
      sma_target_position,
      sma_rules,
      replacements
    ) {

      base_



    }




  )
)
