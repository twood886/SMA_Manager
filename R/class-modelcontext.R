ModelContext <- R6::R6Class( #nolint
  "ModelContext",
  public = list(
    n = NULL,
    ids = NULL,
    price = NULL,
    nav = NULL,
    t_w = NULL,
    sgn = NULL,
    w = NULL,
    alpha = NULL,
    params = NULL,
    index_of = NULL,
    var_factory = NULL,
    initialize = function(
      n, ids, price, nav, t_w, sgn, w, alpha, params, index_of, var_factory
    ) {
      self$n <- n
      self$ids <- ids
      self$price <- price
      self$nav <- nav
      self$t_w <- t_w
      self$sgn <- sgn
      self$w <- w
      self$alpha <- alpha
      self$params <- params
      self$index_of <- index_of
      self$var_factory <- var_factory
    }
  )
)