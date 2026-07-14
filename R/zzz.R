#' The master registries environment
#'
#' This environment (and its sub‐environments) is created on package load.
#' @export
registries <- NULL

# Package-level mutable state (connection cache, etc.) — not exported.
.pkg_state <- NULL

.onLoad <- function(libname, pkgname) {
  registries  <<- new.env(parent = emptyenv())
  registries$portfolios <<- new.env(parent = emptyenv())
  registries$securities <<- new.env(parent = emptyenv())
  registries$smarules   <<- new.env(parent = emptyenv())
  registries$trades     <<- new.env(parent = emptyenv())

  .pkg_state      <<- new.env(parent = emptyenv())
  .pkg_state$con  <<- NULL
  .pkg_state$security_data_provider <<- NULL
}
