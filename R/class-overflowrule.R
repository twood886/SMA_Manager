#' OverflowRule
#' @export
OverflowRule <- R6::R6Class(
  "OverflowRule",
  private = list(
    replacements_ = NULL
  ),
  public = list(
    #' @description Create a new OverflowRule R6 object.
    #' @param replacements Named list of replacements. Each name is a source
    #' security ID, and each element is a list with \code{security} (a
    #' character vector of target security IDs) and \code{weight} (a numeric
    #' vector of the same length giving each target's fixed share of the
    #' source's overflow; must sum to 1).
    initialize = function(replacements) {
      private$replacements_ <- replacements
    },
    #' @description Build CVXR constraints for the rule
    #' @param ctx Context object with optimization variables and parameters
    build_constraints = function(ctx) {
      cons <- list()
      ids <- ctx$ids
      t_w <- ctx$t_w
      w <- ctx$w
      a <- ctx$alpha

      if (!length(private$replacements_)) return(cons)

      for (src in names(private$replacements_)) {
        i <- match(src, ids)
        entry <- private$replacements_[[src]]
        tgt_ids <- as.character(entry$security)
        tgt_weight <- as.numeric(entry$weight)
        js <- match(tgt_ids, ids)
        keep <- !is.na(js)
        js <- js[keep]
        tgt_weight <- tgt_weight[keep]
        if (is.na(i) || !length(js)) next
        # Targets absent from this optimization's universe drop out; rescale
        # the remaining weights so the present targets still absorb all of
        # the source's overflow.
        tgt_weight <- tgt_weight / sum(tgt_weight)

        # direction clamps
        cons <- c(
          cons,
          list(
            if (t_w[i] >= 0) {
              w[i] <= a * t_w[i]
            } else {
              w[i] >= a * t_w[i]
            }
          )
        )

        overflow <- a * t_w[i] - w[i]
        for (k in seq_along(js)) {
          j <- js[k]
          cons <- c(
            cons,
            list(if (t_w[j] >= 0) w[j] >= a * t_w[j] else w[j] <= a * t_w[j])
          )
          # Each target absorbs a fixed share of the source's overflow,
          # rather than an amount chosen freely by the optimizer.
          cons <- c(
            cons,
            list((w[j] - a * t_w[j]) == tgt_weight[k] * overflow)
          )
        }
      }
      cons
    },
    #' @description Objective terms contributed by this rule (Dummy)
    #' @param ctx Context object with optimization variables and parameters
    #' @return An empty list
    objective_terms = function(ctx) list()
  )
)