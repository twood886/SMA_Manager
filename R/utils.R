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
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
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
#' @throws An error if the object does not inherit from the specified class.
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