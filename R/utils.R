#' Null/empty coalescing
#' Returns y if x is NULL, length 0, or NA; otherwise returns x.
#' @param x An object to check.
#' @param y A fallback value to return if x is NULL, length 0, or NA.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

#' Assert that an input is a single non-missing string
#'
#' This function checks whether the input `x` is a character vector of length 1
#' and is not `NA`. If the input does not meet these criteria, an error is raised.
#'
#' @param x The input to check. Expected to be a single string.
#' @param name A string representing the name of the input, used in the error message.
#'
#' @return This function does not return a value. It is used for its side effect
#' of throwing an error if the input is invalid.
#'
#' @examples
#' assert_string("example", "input_name") # Passes without error
#' # assert_string(123, "input_name") # Throws an error
#' # assert_string(c("a", "b"), "input_name") # Throws an error
#' # assert_string(NA, "input_name") # Throws an error
assert_string <- function(x, name) {
  if (any(!is.character(x) ,is.na(x))) {
    stop(sprintf("%s must be a non-missing single string", name), call. = FALSE)
  }
}

#' Assert Object Inherits from a Specific Class
#'
#' This function checks whether an object inherits from a specified class. 
#' If the object does not inherit from the specified class, an error is raised.
#'
#' @param x The object to check.
#' @param class A character string specifying the class to check against.
#' @param name A character string representing the name of the object (used in the error message).
#'
#' @return This function does not return a value. It is called for its side effect of throwing an error if the check fails.
#'
#' @examples
#' # Example usage:
#' my_list <- list(a = 1, b = 2)
#' assert_inherits(my_list, "list", "my_list") # Passes without error
#'
#' # This will throw an error:
#' # assert_inherits(my_list, "data.frame", "my_list")
#'
assert_inherits <- function(x, class, name) {
  if (!inherits(x, class)) {
    stop(sprintf("%s must be a %s object", name, class), call. = FALSE)
  }
}

#' Assert Numeric
#'
#' This function checks if the input is a single, non-missing numeric value.
#' If the input does not meet these criteria, an error is raised.
#'
#' @param x The value to check.
#' @param name A character string representing the name of the variable being checked.
#'   This is used in the error message for better debugging.
#'
#' @return None. The function is used for its side effect of throwing an error
#'   if the input does not meet the criteria.
#'
#' @examples
#' assert_numeric(5, "my_var")  # Passes without error
#' assert_numeric("a", "my_var")  # Throws an error
#' assert_numeric(NA, "my_var")  # Throws an error
assert_numeric <- function(x, name) {
  if (any(!is.numeric(x) ,is.na(x))) {
    stop(sprintf("%s must be a non-missing single numeric", name), call. = FALSE)
  }
}


#' Assert Boolean Value
#'
#' This function checks if the input is a single, non-missing logical value.
#' If the input does not meet these criteria, an error is raised.
#'
#' @param x The value to check.
#' @param name A character string representing the name of the variable being checked.
#'   This is used in the error message for better debugging.
#'
#' @return None. The function is used for its side effect of throwing an error
#'   if the input does not meet the criteria.
#'
#' @examples
#' assert_bool(TRUE, "my_var")  # Passes validation
#' assert_bool(FALSE, "my_var") # Passes validation
#' \dontrun{
#' assert_bool(NA, "my_var")    # Throws an error
#' assert_bool(1, "my_var")     # Throws an error
#' assert_bool(c(TRUE, FALSE), "my_var") # Throws an error
#' }
assert_bool <- function(x, name) {
  if (any(!is.logical(x), is.na(x))) {
    stop(sprintf("%s must be a non-missing single logical", name), call. = FALSE)
  }
}

#' Retrieve Registries from Package Namespace
#'
#' This function accesses the `registries` object from the namespace of the 
#' specified package (`yourpkg`). It assumes that the `registries` object 
#' exists within the package's namespace.
#'
#' @return The `registries` object from the namespace of the package `yourpkg`.
#' @examples
#' \dontrun{
#'   registries <- get_registries()
#' }
#' 
#' @export
get_registries <- function() {
  asNamespace("SMAManager")$registries
}


#' Update Data in all Security Object
#' @export
update_security_data <- function() {
  security_ids <- ls(get_registries()$securities)
  type <- vapply(security_ids, function(id) .security(id)$get_instrument_type(), character(1)) #nolint
  price <- Rblpapi::bdp(security_ids, "PX_LAST")
  price[type == "FixedIncome", "PX_LAST"] <- price[type == "FixedIncome", "PX_LAST"] / 100
  delta <- Rblpapi::bdp(security_ids, "OP006")
  delta[type != "Option", "OP006"] <- 1
  lapply(
    security_ids,
    function(id) {
      security <- .security(id)
      security$set_price(price[id, "PX_LAST"])
      security$set_delta(delta[id, "OP006"])
      security$update_underlying_price()
    }
  )
  invisible(NULL)
}

#' Add Bloomberg Data for SMA Rules to Securities
#' @return TRUE (invisible)
#' @export
#' @include api-functions.R
#' @include class-security.R
update_bloomberg_fields <- function() {
  rule_names <- ls(get_registries()$smarules)
  rules <- mget(
    rule_names,
    envir = get_registries()$smarules,
    inherits = TRUE
  )
  rule_bbfields <- unique(
    unlist(
      sapply(
        rules,
        function(rule) rule$get_bbfields(),
        simplify = TRUE
      ),
      use.names = FALSE
    )
  )
  security_ids <- ls(get_registries()$securities)
  bbdata <- Rblpapi::bdp(
    security_ids,
    fields = rule_bbfields
  )
  for (col in seq_len(ncol(bbdata))) {
    field <- colnames(bbdata)[col]
    for (row in seq_len(nrow(bbdata))) {
      id <- rownames(bbdata)[row]
      value <- bbdata[row, col]
      .security(id, create = FALSE)$set_rule_data(field, value)
    }
  }
  invisible(TRUE)
}


#' Check Rule Compliance
#' @param portfolio_name Character. Name of the portfolio to check.
#' @param update_bbfields Logical. Whether to update Bloomberg data before checking rules. Defaults to TRUE.
#' @export
check_rule_compliance <- function(portfolio_name, update_bbfields = TRUE) {
  assert_string(portfolio_name, "portfolio_name")
  portfolio <- get(portfolio_name, envir = get_registries()$portfolios, inherits = FALSE) #nolint
  assert_inherits(portfolio, "Portfolio", "portfolio")

  check <- portfolio$check_rules_current(update_bbfields = update_bbfields)
  non_comply <- which(sapply(check, function(x) !x$pass))
  if (length(non_comply) == 0) {
    return(list("pass" = TRUE, "message" = "All rules are compliant."))
  } else {
    rule_names <- names(rules)[non_comply]
    messages <- sapply(check[non_comply], function(x) x$message)
    return(list("pass" = FALSE, "message" = paste(rule_names, collapse = ", "), "details" = messages))
  }
}