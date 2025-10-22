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
    #' security ID, and each element is a character vector of target security
    #' IDs.
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
        tgt_ids <- as.character(private$replacements_[[src]])
        js <- match(tgt_ids, ids)
        js <- js[!is.na(js)]
        if (is.na(i) || !length(js)) next

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

        for (j in js) {
          cons <- c(
            cons,
            list(if (t_w[j] >= 0) w[j] >= a * t_w[j] else w[j] <= a * t_w[j])
          )
        }
        # conservation: (alpha t_w_src - w_src) == sum_j (w_j - alpha t_w_j)
        cons <- c(
          cons,
          list((a * t_w[i] - w[i]) == CVXR::sum_entries(w[js] - a * t_w[js]))
        )
      }
      cons
    },
    #' @description Objective terms contributed by this rule (Dummy)
    #' @param ctx Context object with optimization variables and parameters
    #' @return An empty list
    objective_terms = function(ctx) list()
  )
)