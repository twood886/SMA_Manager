.onLoad <- function(libname, pkgname) {
  # Assign the environment to the package namespace
  .sma_registry <<- new.env(parent = emptyenv())
  .portfolio_registry <<- new.env(parent = emptyenv())
  .security_registry <<- new.env(parent = emptyenv())
  .smarule_registry <<- new.env(parent = emptyenv())
  .trade_registry <<- new.env(parent = emptyenv())
}