% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-functions.R
\name{.trade}
\alias{.trade}
\title{Create or Retrieve a Trade Object}
\usage{
.trade(
  security_id,
  portfolio_id,
  qty,
  swap,
  create = FALSE,
  assign_to_registry = TRUE
)
}
\arguments{
\item{security_id}{A string representing the ID of the security. Must be a valid string.}

\item{portfolio_id}{A string representing the ID of the portfolio. Must be a valid string.}

\item{qty}{A numeric value representing the quantity of the trade. Must be a valid numeric value.}

\item{swap}{A boolean indicating whether the trade is a swap. Must be `TRUE` or `FALSE`.}

\item{create}{A boolean indicating whether to create a new trade if it does not exist. Must be `TRUE` or `FALSE`.}

\item{assign_to_registry}{A boolean indicating whether to assign the trade object to the registry. Defaults to `TRUE`.}
}
\value{
If `create` is `FALSE`, returns a list of existing trades for the given security and swap flag.
        If `create` is `TRUE`, returns the trade object after updating its quantity and the portfolio's target position.
}
\description{
This function manages trades for a given security and portfolio. It retrieves or creates a trade object,
updates the trade quantity, and adjusts the target position in the portfolio accordingly.
}
\details{
The function first validates the input parameters. It retrieves all existing trades from the `registries$trades`
environment and filters them based on the `security_id` and `swap` flag. If `create` is `FALSE`, it returns the
filtered trades. If `create` is `TRUE`, it creates a new trade if none exists, updates the trade quantity, and
adjusts the target position in the portfolio.
}
\examples{
# Example usage:
# Retrieve existing trades
trades <- .trade("SEC123", "PORT456", qty = 0, swap = FALSE, create = FALSE)

# Create or update a trade
trade <- .trade("SEC123", "PORT456", qty = 100, swap = FALSE, create = TRUE)

}
\seealso{
\code{\link{Trade}}, \code{\link{Portfolio}}
}
