% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-functions.R
\name{.portfolio}
\alias{.portfolio}
\title{Create or Retrieve a Portfolio Object}
\usage{
.portfolio(
  short_name,
  long_name,
  holdings_url,
  trade_url,
  nav = 0,
  positions = list(),
  create = FALSE,
  assign_to_registry = TRUE
)
}
\arguments{
\item{short_name}{A string representing the short name of the portfolio.
Must be unique within the portfolio registry.}

\item{long_name}{A string representing the long name of the portfolio.
Required if creating a new portfolio.}

\item{holdings_url}{A string representing the URL for holdings data.}

\item{nav}{A numeric value representing the net asset value (NAV) of the
portfolio. Defaults to 0.}

\item{positions}{A list of positions to initialize the portfolio with.
Each position must inherit from the "Position" class. Defaults to an
empty list.}

\item{create}{A logical value indicating whether to create the portfolio
if it does not exist. Defaults to `FALSE`.}

\item{assign_to_registry}{A logical value indicating whether to assign the
portfolio object to the registry. Defaults to `TRUE`.}
}
\value{
If the portfolio exists or is successfully created, the portfolio
  object is returned. Otherwise, an error is raised.
}
\description{
This function creates a new portfolio object or retrieves an existing one
from the portfolio registry. If the portfolio does not exist and `create`
is set to `FALSE`, an error is raised.
}
\details{
The function checks for the existence of a portfolio in the
  `registries$portfolios` environment using the `short_name`. If the
  portfolio exists, it is retrieved and returned. If it does not exist
  and `create` is `TRUE`, a new portfolio is created. If `assign_to_registry`
  is true, the object is added to the registry.
  The function performs various assertions to ensure the
  validity of the input arguments.
}
\examples{
# Retrieve an existing portfolio
.portfolio("short_name")

# Create a new portfolio
.portfolio(
 "short_name",
 "Long Name",
 nav = 1000,
 positions = list(),
 create = TRUE
)

}
\seealso{
\code{\link{Portfolio}} for the Portfolio class.
}
