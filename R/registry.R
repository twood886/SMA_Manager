# registry.R

# Create the environment that will hold all Security objects by ticker
.security_registry <- new.env(parent = emptyenv())