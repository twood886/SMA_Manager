# registry.R

# Create the environment that will hold all Security objects by ticker
.security_registry <- new.env(parent = emptyenv())
# Create the environment that will hold all Portfolios objects by id
.portfolio_registry <- new.env(parent = emptyenv())
# Create the environment that will hold all SMA objects by id
.sma_registry <- new.env(parent = emptyenv())