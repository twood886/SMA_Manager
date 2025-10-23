#' @title SMA Rule for Position Count
#' @description R6 Class that encapsulates rule for SMAs based on position count. #nolint
#'
#' @import R6
#' @include class-smarule.R
#' @export
SMARuleCount <- R6::R6Class( #nolint
  "SMARuleCount",
  inherit = SMARule,
  private = list(
    side_ = NULL
  ),
  public = list(
    #' @param sma_name Character
    #' @param name Character
    #' @param scope Character
    #' @param bbfields Character vector
    #' @param definition Formula
    #' @param max_threshold numeric
    #' @param min_threshold numeirc
    #' @param swap_only logical
    #' @param gross_exposure logical
    #' @param relative_to Character
    #' @param divisor DivisorProvider object
    #' @param side character
    initialize = function(
      sma_name = NULL,
      name = NULL,
      scope = "count",
      bbfields = NULL,
      definition = NULL,
      max_threshold = NULL,
      min_threshold = NULL,
      swap_only = FALSE,
      gross_exposure = FALSE,
      relative_to = NULL,
      divisor = NULL,
      side = NULL
    ) {
      side <- tolower(side)
      super$initialize(
        sma_name = sma_name,
        name = name,
        scope = scope,
        bbfields = bbfields,
        definition = definition,
        max_threshold = max_threshold,
        min_threshold = min_threshold,
        swap_only = swap_only,
        gross_exposure = gross_exposure,
        relative_to = relative_to,
        divisor = divisor
      )
      if (isTRUE(gross_exposure)) side <- "gross"
      if (!side %in% c("long", "short", "gross")) {
        stop("If not gross, side must be long or short")
      }
      private$side_ <- side
    },

    #' @description Get Side argument
    get_side = function() private$side_,

    #' @description Check the rule against the current portfolio
    #' @param sma SMA object
    #' @param tolerance Numerical tolerance for constrain checking
    check_compliance = function(sma, tolerance = 1e-6) {
      checkmate::assert_r6(sma, "Portfolio")
      positions <- sma$get_position()
      qty <- vapply(positions, \(p) p$get_qty(), numeric(1))
      side <- self$get_side()

      n <- if (!length(qty)) 0 else switch(
        side,
        "gross" = sum(abs(qty) > tolerance),
        "long" = sum(qty > tolerance),
        "short" = sum(qty < -tolerance)
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

    #' @description Get the Max and Min Value of the security based on the rule
    #' @param security_id Security ID
    #' @param sma SMA object
    get_security_limits = function(security_id, sma) {
      out <- replicate(
        length(security_id),
        list(list(max = Inf, min = -Inf)),
        simplify = FALSE
      )
      names(out) <- security_id
      out
    },

    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    #' @param sma SMA object
    build_constraints = function(ctx, sma) {
      w <- ctx$w
      n <- ctx$n
      side <- self$get_side()
      min_t <- self$get_min_threshold()
      max_t <- self$get_max_threshold()

      eps <- 1e-4

      cons <- list()

      if (side == "long") {
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

      if (side == "short") {
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

      if (side == "gross") {
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