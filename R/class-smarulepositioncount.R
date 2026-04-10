#' @title SMA Rule for Position Count
#' @description R6 Class that encapsulates rule for SMAs based on position count. #nolint
#'
#' @import R6
#' @include class-smarule.R
#' @export
SMARuleCount <- R6::R6Class( #nolint
  "SMARuleCount",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against raw portfolio data
    #' @param ids Character vector of security IDs
    #' @param qty Numeric vector of quantities
    #' @param nav Numeric NAV value
    #' @param prices Optional numeric vector of prices. If NULL, fetched.
    #' @param tolerance Numerical tolerance for constraint checking
    #' @param ... Additional arguments (not used)
    check_compliance = function(
      ids, qty, nav, prices = NULL, tolerance = 1e-6, ...
    ) {
      side <- self$get_include()

      n <- if (!length(qty)) 0 else switch(
        side,
        "all" = sum(abs(qty) > tolerance),
        "long_only" = sum(qty > tolerance),
        "short_only" = sum(qty < -tolerance)
      )

      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()
      ok_min <- !(is.finite(min_t)) || (n >= min_t)
      ok_max <- !(is.finite(max_t)) || (n <= max_t)

      if (ok_min && ok_max) {
        list(pass = TRUE)
      } else {
        list(
          pass = FALSE,
          violates_max = n > max_t,
          violates_min = n < min_t,
          non_comply = NULL,
          value = NULL,
          divisor_kind = NULL,
          divisor_value = NULL
        )
      }
    },

    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      vapply(security_id, \(x) FALSE, logical(1))
    },

    #' @description Get limits for securities based on raw portfolio data
    #' @param security_id Vector of security IDs to calculate limits for
    #' @param ids_all Character vector of all security IDs (must include
    #'  security_id). For new securities not in portfolio, include with qty=0.
    #' @param qty_all Numeric vector of quantities corresponding to ids_all
    #' @param nav Numeric NAV value
    #' @param prices_all Optional numeric vector of prices corresponding to
    #'  ids_all. If NULL, fetched via .security().
    #' @param f_all Optional numeric vector of rule values for ids_all.
    #'  If NULL, computed via apply_rule_definition.
    #' @return (Returns Inf/-Inf)
    get_security_limits = function(
      security_id, ids_all, qty_all, nav, prices_all = NULL, f_all = NULL
    ) {
      out <- replicate(
        length(security_id),
        list(max = Inf, min = -Inf),
        simplify = FALSE
      )
      names(out) <- security_id
      out
    },

    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    #' @param nav Portfolio NAV
    build_constraints = function(ctx, nav) {
      w <- ctx$w
      n <- ctx$n
      side <- self$get_include()
      min_t <- self$get_min_threshold()
      max_t <- self$get_max_threshold()

      eps <- 1e-4

      cons <- list()

      if (side == "long_only") {
        z <- CVXR::Variable(n, boolean = TRUE, name = paste0("z_long_", self$get_name())) #nolint
        p <- CVXR::Variable(n, name = paste0("p_long_", self$get_name()))
        cons <- c(cons,
          list(p >= w),
          list(p >= 0),
          list(p >= eps * z)
        )
        if (is.finite(min_t)) cons <- c(cons, list(CVXR::sum_entries(z) >= min_t)) #nolint
        if (is.finite(max_t)) cons <- c(cons, list(CVXR::sum_entries(z) <= max_t)) #nolint
        return(cons)
      }

      if (side == "short_only") {
        z <- CVXR::Variable(n, boolean = TRUE, name = paste0("z_short_", self$get_name())) #nolint
        s <- CVXR::Variable(n, name = paste0("s_short_", self$get_name()))
        cons <- c(cons,
          list(s >= -w),
          list(s >= 0),
          list(s >= eps * z)
        )
        if (is.finite(min_t)) cons <- c(cons, list(CVXR::sum_entries(z) >= min_t)) #nolint
        if (is.finite(max_t)) cons <- c(cons, list(CVXR::sum_entries(z) <= max_t)) #nolint
        return(cons)
      }

      if (side == "all") {
        z_long <- CVXR::Variable(n, boolean = TRUE, name = paste0("z_long_", self$get_name())) #nolint
        z_short <- CVXR::Variable(n, boolean = TRUE, name = paste0("z_short_", self$get_name())) #nolint
        y <- CVXR::Variable(n, boolean = TRUE, name = paste0("y_gross_", self$get_name())) #nolint
        p <- CVXR::Variable(n, name = paste0("p_gross_pos_", self$get_name()))
        s <- CVXR::Variable(n, name = paste0("s_gross_neg_", self$get_name()))
        cons <- c(cons,
          list(p >= w), list(p >= 0),
          list(s >= -w), list(s >= 0),
          list(p >= eps * z_long),
          list(s > eps * z_short),
          list(y >= z_long, y >= z_short),
          list(y <= z_long + z_short)
        )
        if (is.finite(min_t)) cons <- c(cons, list(CVXR::sum_entries(y) >= min_t)) #nolint
        if (is.finite(max_t)) cons <- c(cons, list(CVXR::sum_entries(y) <= max_t)) #nolint
        return(cons)
      }
      stop("Unrecognized side in SMARuleCount: ", side)
    }
  )
)