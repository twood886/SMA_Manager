% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_registries}
\alias{get_registries}
\title{Retrieve Registries from Package Namespace}
\usage{
get_registries()
}
\value{
The `registries` object from the namespace of the package `yourpkg`.
}
\description{
This function accesses the `registries` object from the namespace of the 
specified package (`yourpkg`). It assumes that the `registries` object 
exists within the package's namespace.
}
\examples{
\dontrun{
  registries <- get_registries()
}

}
