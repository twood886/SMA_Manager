.onLoad <- function(libname, pkgname) {
  # Assign the environment to the package namespace
  registries <<- new.env(parent = emptyenv())
  registries$portfolios <<- new.env(parent = emptyenv())
  registries$securities <<- new.env(parent = emptyenv())
  registries$smarules <<- new.env(parent = emptyenv())
  registries$trades <<- new.env(parent = emptyenv())
}