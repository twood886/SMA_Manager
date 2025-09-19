VariableFactory <- R6::R6Class( #nolint
  "VariableFactory",
  public = list(
    cache = NULL,
    initialize = function() self$cache <- new.env(parent = emptyenv()),
    vec = function(key, len, name = NULL) {
      k <- paste0(key, ":", len)
      if (!exists(k, envir = self$cache, inherits = FALSE)) {
        vname <- name %||% key
        self$cache[[k]] <- CVXR::Variable(len, name = vname)
      }
      self$cache[[k]]
    },
    scalar = function(key, name = NULL) {
      if (!exists(key, envir = self$cache, inherits = FALSE)) {
        vname <- name %||% key
        self$cache[[key]] <- CVXR::Variable(1, name = vname)
      }
      self$cache[[key]]
    }
  )
)