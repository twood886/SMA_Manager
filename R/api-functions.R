#' Retrieve or Create a Security Object
#'
#' This function retrieves a security object from the securities registry by its Bloomberg ID (bbid).
#' If the security does not exist in the registry and `create` is set to `TRUE`, it attempts to create
#' a new security object using Bloomberg data.
#'
#' @param bbid A string representing the Bloomberg ID of the security. Must be a non-empty string.
#' @param create A logical value indicating whether to create a new security object if it does not
#'   already exist in the registry. Defaults to `TRUE`.
#'
#' @return If the security exists in the registry, it returns the corresponding security object.
#'   If the security does not exist and `create` is `FALSE`, it returns `NULL`. If `create` is `TRUE`,
#'   it creates a new security object, adds it to the registry, and returns it.
#'
#' @details The function first validates the `bbid` parameter to ensure it is a valid string. It then
#'   checks if the security exists in the `registries$securities` environment. If the security does not
#'   exist and `create` is `TRUE`, it queries Bloomberg for the security's data using the `Rblpapi::bpd`
#'   function. If the Bloomberg data indicates the security is not found, an error is raised. Otherwise,
#'   a new `Security` object is created and added to the registry.
#'
#' @examples
#' # Retrieve an existing security
#' security("AAPL US Equity")
#'
#' # Create a new security if it does not exist
#' security("MSFT US Equity", create = TRUE)
#'
#' # Attempt to retrieve a security without creating it
#' security("GOOG US Equity", create = FALSE)
#' 
#' @seealso \code{\link{Security}} for the Security class.
#'
#' @importFrom Rblpapi bdp
#' @export
.security <- function(bbid, create = TRUE) { 
  assert_string(bbid, "bbid")
  bbid <- tolower(bbid)
  env <- registries$securities
  if (exists(bbid, envir = env, inherits = FALSE)) return(get(bbid, envir = env))
  if (!create) return(NULL)
  if (Rblpapi::bdp(bbid, "DX194")$DX194 == "") stop("Security not found in Bloomberg")
  security <- Security$new(bbid)
  assign(bbid, security, envir = env)
  security
}

#' Create or Retrieve a Portfolio Object
#'
#' This function creates a new portfolio object or retrieves an existing one 
#' from the portfolio registry. If the portfolio does not exist and `create` 
#' is set to `FALSE`, an error is raised.
#'
#' @param short_name A string representing the short name of the portfolio. 
#'   Must be unique within the portfolio registry.
#' @param long_name A string representing the long name of the portfolio. 
#'   Required if creating a new portfolio.
#' @param nav A numeric value representing the net asset value (NAV) of the 
#'   portfolio. Defaults to 0.
#' @param positions A list of positions to initialize the portfolio with. 
#'   Each position must inherit from the "Position" class. Defaults to an 
#'   empty list.
#' @param create A logical value indicating whether to create the portfolio 
#'   if it does not exist. Defaults to `FALSE`.
#'
#' @return If the portfolio exists or is successfully created, the portfolio 
#'   object is returned. Otherwise, an error is raised.
#'
#' @details The function checks for the existence of a portfolio in the 
#'   `registries$portfolios` environment using the `short_name`. If the 
#'   portfolio exists, it is retrieved and returned. If it does not exist 
#'   and `create` is `TRUE`, a new portfolio is created and added to the 
#'   registry. The function performs various assertions to ensure the 
#'   validity of the input arguments.
#'
#' @examples
#' # Retrieve an existing portfolio
#' portfolio("short_name")
#'
#' # Create a new portfolio
#' portfolio("short_name", "Long Name", nav = 1000, positions = list(), create = TRUE)
#'
#' @seealso \code{\link{Portfolio}} for the Portfolio class.
#'
#' @export
.portfolio <- function(short_name, long_name, nav = 0, positions = list(), create = FALSE) {
  assert_string(short_name, "short_name")
  assert_bool(create, "create")
  env <- registries$portfolios
  if (exists(short_name, envir=env, inherits=FALSE)) return(get(short_name, envir=env))
  if (!create) stop("Portfolio does not exist and create is set to FALSE")
  assert_string(long_name, "long_name")
  assert_numeric(nav, "nav")
  lapply(positions, function(position) assert_inherits(position, "Position", "positions"))
  portfolio <- Portfolio$new(long_name, short_name, nav, positions)
  assign(short_name, portfolio, envir=env)
  portfolio
}


#' Create or Update a Position in a Portfolio
#'
#' This function creates or updates a position in a specified portfolio. It ensures
#' that the input parameters are valid and initializes a new position object.
#'
#' @param portfolio_name A string specifying the name of the portfolio. The portfolio
#'   must already exist.
#' @param bbid A string representing the Bloomberg identifier (BBID) of the security.
#'   This will be converted to lowercase.
#' @param qty A numeric value indicating the quantity of the position. Defaults to 0.
#' @param swap A logical value (TRUE or FALSE) indicating whether the position is a swap.
#'   Defaults to FALSE.
#'
#' @return An object of class `Position` representing the created or updated position.
#'
#' @details The function validates the input parameters using assertion checks. If the
#'   security does not already exist, it will be created. The position is then initialized
#'   using the `Position$new` method.
#'
#' @examples
#' # Create a position with 100 shares of a security
#' position("MyPortfolio", "AAPL US Equity", qty = 100)
#'
#' # Create a swap position
#' position("MyPortfolio", "AAPL US Equity", qty = 50, swap = TRUE)
#'
#' @seealso \code{\link{Position}} for the Position class.
#' 
#' @export
.position <- function(portfolio_name, bbid, qty = 0, swap = FALSE) {
  assert_string(portfolio_name, "portfolio_name")
  assert_string(bbid, "id")
  assert_numeric(qty, "qty")
  assert_bool(swap, "swap")
  bbid <- tolower(bbid) 
  sec <- .security(bbid, create = TRUE)
  Position$new(portfolio_name, sec, qty, swap)
}
