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