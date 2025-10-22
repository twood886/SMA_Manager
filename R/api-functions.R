#' Retrieve or Create a Security Object
#'
#' This function retrieves a security object from the securities registry by its
#'  Bloomberg ID (bbid). If the security does not exist in the registry and
#'  `create` is set to `TRUE`, it attempts to create a new security object
#'  using Bloomberg data.
#'
#' @param sec_id A string representing the Bloomberg ID of the security.
#'  Must be a non-empty string.
#' @param create A logical value indicating whether to create a new security
#'  object if it does not already exist in the registry. Defaults to `TRUE`.
#' @param assign_to_registry A logical value indicating whether to assign
#'  the security object to the registry. Defaults to `TRUE`.
#'
#' @return If the security exists in the registry, it returns the corresponding
#'  security object. If the security does not exist and `create` is `FALSE`, it
#'  returns `NULL`. If `create` is `TRUE`, it creates a new security object and
#'  returns it. If `assign_to_registry` is `TRUE`, the new object is assigned to
#'  the registry.
#'
#' @details The function first validates the `bbid` parameter to ensure it is a
#'  valid string. It then checks if the security exists in the
#'  `registries$securities` environment. If the security does not exist and
#'  `create` is `TRUE`, it queries Bloomberg for the security's data using the
#'  `Rblpapi::bpd` function. If the Bloomberg data indicates the security is not
#'  found, an error is raised. Otherwise, a new `Security` object is created and
#'  added to the registry.
#'
#' @examples
#' # Retrieve an existing security
#' security("AAPL US Equity")
#'
#' # Create a new security if it does not exist
#' security("MSFT US Equity", create = TRUE, assign_to_registry = TRUE)
#'
#' # Attempt to retrieve a security without creating it
#' security("GOOG US Equity", create = FALSE,  assign_to_registry = FALSE)
#'
#' @seealso \code{\link{Security}} for the Security class.
#'
#' @importFrom Rblpapi bdp
#' @import checkmate
#' @export
.security <- function(sec_id, create = TRUE, assign_to_registry = TRUE) {
  checkmate::assert_character(sec_id)
  checkmate::assert_logical(create)
  checkmate::assert_logical(assign_to_registry)
  sec_id <- tolower(sec_id)
  env <- registries$securities
  if (exists(sec_id, envir = env, inherits = FALSE)) {
    return(get(sec_id, envir = env))
  }
  if (!create) return(NULL)
  if (Rblpapi::bdp(sec_id, "DX194")$DX194 == "") {
    stop("Security not found in Bloomberg")
  }
  security <- Security$new(sec_id)
  if (assign_to_registry) {
    assign(sec_id, security, envir = env)
  }
  invisible(security)
}

#' Create or Retrieve a Holding Object
#'
#' This function creates or retrieves a holding object associated with a
#' specific portfolio and security. If the holding already exists, it is
#' returned. If not, a new holding is created and added to the corresponding
#' position in the portfolio.
#'
#' @param portfolio_name A string representing the name of the portfolio.
#' Must be a valid portfolio name.
#' @param sec_id A string representing the security ID (ticker).
#' Must be a valid security ID.
#' @param qty A numeric value representing the quantity of the holding.
#' Must be a valid numeric value.
#' @param swap A logical value indicating whether the holding is a swap.
#' Defaults to `FALSE`.
#' @param custodian (Optional) A string representing the custodian name.
#' @param broker_custodian (Optional) A string representing the broker custodian name. #nolint
#' @param custodian_acct_id (Optional) A string representing the custodian account ID. #nolint
#' @param custodian_acct (Optional) A string representing the custodian account name. #nolint
#' @param trs_custodian_id (Optional) A string representing the TRS custodian ID. #nolint
#' @param trs_custodian_name (Optional) A string representing the TRS custodian name. #nolint
#' @param assign_to_portfolio A logical value indicating whether to add the holding. #nolint
#'
#' @return An object of class `Holding` representing the created or retrieved holding. #nolint
#'
#' @details The function first validates the input parameters to ensure they are
#' of the correct type. It then constructs a unique ID for the holding based on
#' the security ID and custodian account ID. The function checks if the holding
#' already exists in the position associated with the portfolio. If it exists,
#' the existing holding is returned. If not, a new `Holding` object is created
#' and added to the position.
#'
#' @seealso \code{\link{Holding}} for the Holding class.
#' @import checkmate
#' @export
.holding <- function(
  portfolio_name, sec_id, qty, swap = FALSE,
  custodian = NULL, broker_custodian = NULL,
  custodian_acct_id = NULL, custodian_acct = NULL,
  trs_custodian_id = NULL, trs_custodian_name = NULL,
  create = FALSE, assign_to_portfolio = TRUE
) {
  checkmate::assert_character(sec_id)
  sec_id <- tolower(sec_id)
  checkmate::assert_numeric(qty)
  checkmate::assert_flag(swap)
  checkmate::assert_character(custodian, null.ok = TRUE)
  checkmate::assert_character(broker_custodian, null.ok = TRUE)
  checkmate::assert_character(custodian_acct_id, null.ok = TRUE)
  checkmate::assert_character(custodian_acct, null.ok = TRUE)
  checkmate::assert_character(trs_custodian_id, null.ok = TRUE)
  checkmate::assert_character(trs_custodian_name, null.ok = TRUE)
  id <- paste(sec_id, custodian_acct_id, sep = "|")

  portfolio <- .portfolio(portfolio_name, create = FALSE)
  position <- .position(portfolio_name, sec_id, TRUE, assign_to_portfolio)
  holdings <- position$get_holdings()
  holdings_id <- vapply(holdings, \(x) x$get_id(), character(1))

  holding <- tryCatch(
    holdings[[which(holdings_id == id)]],
    error = function(e) NULL
  )
  if (!is.null(holding) && create) holding$set_qty(qty)
  if (!is.null(holding)) return(invisible(holding))
  if (!create) stop("Holding does not exist and create is set to FALSE")

  holding <- Holding$new(
    portfolio_name, sec_id, qty, swap,
    custodian, broker_custodian,
    custodian_acct_id, custodian_acct,
    trs_custodian_id, trs_custodian_name
  )
  position$add_holding(holding)

  if (assign_to_portfolio) {
    portfolio$add_position(position, overwrite = TRUE)
  }
  return(invisible(holding))
}

#' Create or Retrieve a Position Object
#'
#' This function creates or updates a position in a specified portfolio.
#'  It ensures that the input parameters are valid and initializes a new
#'  position object.
#'
#' @param portfolio_name A string specifying the name of the portfolio.
#'  The portfolio must already exist.
#' @param sec_id A string representing the Bloomberg identifier (BBID) of the
#'  security. This will be converted to lowercase.
#' @param create A logical value indicating whether to create the position
#' @param assign_to_portfolio A logical value indicating whether to add the
#'  position to the portfolio. Defaults to `TRUE`.
#'
#' @return An object of class `Position` representing the created or updated
#'  position.
#'
#' @details The function validates the input parameters using assertion checks.
#'  If the security does not already exist, it will be created. The position is
#'  then initialized using the `Position$new` method.
#'
#' @examples
#' # Create a position with 100 shares of a security
#' .position("MyPortfolio", "AAPL US Equity")
#'
#' @seealso \code{\link{Position}} for the Position class.
#' @import checkmate
#'
#' @export
.position <- function(
  portfolio_name, sec_id, create = FALSE, assign_to_portfolio = FALSE
) {
  checkmate::assert_character(portfolio_name)
  checkmate::assert_string(sec_id)
  checkmate::assert_flag(create)
  checkmate::assert_flag(assign_to_portfolio)
  sec_id <- tolower(sec_id)
  portfolio <- .portfolio(portfolio_name, create = FALSE)
  position <- tryCatch(
    portfolio$get_position(sec_id),
    error = function(e) NULL
  )
  if (!is.null(position)) return(invisible(position))
  if (!create) stop("Position does not exist and create is set to FALSE")

  sec <- .security(sec_id, create = TRUE)
  position <- Position$new(portfolio_name, sec)
  if (assign_to_portfolio) {
    portfolio$add_position(position, overwrite = TRUE)
  }
  return(invisible(position))
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
#' @param holdings_url A string representing the URL for holdings data.
#' @param nav A numeric value representing the net asset value (NAV) of the
#'   portfolio. Defaults to 0.
#' @param positions A list of positions to initialize the portfolio with.
#'   Each position must inherit from the "Position" class. Defaults to an
#'   empty list.
#' @param create A logical value indicating whether to create the portfolio
#'   if it does not exist. Defaults to `FALSE`.
#' @param assign_to_registry A logical value indicating whether to assign the
#'  portfolio object to the registry. Defaults to `TRUE`.
#'
#' @return If the portfolio exists or is successfully created, the portfolio
#'   object is returned. Otherwise, an error is raised.
#'
#' @details The function checks for the existence of a portfolio in the
#'   `registries$portfolios` environment using the `short_name`. If the
#'   portfolio exists, it is retrieved and returned. If it does not exist
#'   and `create` is `TRUE`, a new portfolio is created. If `assign_to_registry`
#'   is true, the object is added to the registry.
#'   The function performs various assertions to ensure the
#'   validity of the input arguments.
#'
#' @examples
#' # Retrieve an existing portfolio
#' .portfolio("short_name")
#'
#' # Create a new portfolio
#' .portfolio(
#'  "short_name",
#'  "Long Name",
#'  nav = 1000,
#'  positions = list(),
#'  create = TRUE
#' )
#'
#' @seealso \code{\link{Portfolio}} for the Portfolio class.
#' @import checkmate
#'
#' @export
.portfolio <- function(
  short_name, long_name, holdings_url,
  nav = 0, positions = list(), create = FALSE, assign_to_registry = TRUE
) {
  checkmate::assert_character(short_name)

  env <- registries$portfolios
  if (exists(short_name, envir = env, inherits = FALSE)) {
    return(get(short_name, envir = env))
  }
  checkmate::assert_flag(create)
  checkmate::assert_flag(assign_to_registry)
  checkmate::assert_character(long_name)
  checkmate::assert_numeric(nav)

  if (!create) stop("Portfolio does not exist and create is set to FALSE")
  lapply(positions, \(p) checkmate::assert_r6(p, "Position"))
  portfolio <- Portfolio$new(
    long_name,
    short_name,
    holdings_url,
    nav,
    positions
  )
  if (assign_to_registry) assign(short_name, portfolio, envir = env)
  invisible(portfolio)
}

#' Create or Retrieve an SMA Object
#'
#' This function creates or retrieves an SMA (Separately Managed Account) object
#' If the SMA already exists in the registry, it is returned. Otherwise, a new
#' SMA object is created if the `create` parameter is set to `TRUE`.
#'
#' @param short_name A string representing the short name of the SMA. Must be unique.
#' @param long_name A string representing the long name of the SMA.
#' @param holdings_url A string representing the URL for holdings data.
#' @param nav A numeric value representing the net asset value (NAV) of the SMA. Defaults to 0.
#' @param positions A list of `Position` objects representing the positions in the SMA. Defaults to an empty list.
#' @param base_portfolio A string representing the name of the base portfolio associated with the SMA.
#' @param create A boolean indicating whether to create the SMA if it does not exist. Defaults to `FALSE`.
#' @param assign_to_registry A boolean indicating whether to assign the SMA object to the registry. Defaults to `TRUE`.
#'
#' @return An SMA object.
#' @details The function checks if the SMA with the given `short_name` exists in the 
#' `registries$portfolios` environment. If it exists, the SMA is retrieved and returned. 
#' If it does not exist and `create` is `TRUE`, a new SMA object is created using the 
#' provided parameters and added to the registry. If `create` is `FALSE` and the SMA 
#' does not exist, an error is raised.
#'
#' @examples
#' # Retrieve an existing SMA
#' existing_sma <- .sma("short_name", "long_name", base_portfolio = "base_portfolio")
#'
#' # Create a new SMA
#' new_sma <- .sma("new_short_name", "New Long Name", nav = 1000000, 
#'                 positions = list(position1, position2), 
#'                 base_portfolio = "base_portfolio", create = TRUE)
#'
#' @seealso \code{\link{Portfolio}}, \code{\link{SMA}}
#' @import checkmate
#' @export
.sma <- function(
  short_name, long_name, holdings_url,
  nav = 0, positions = list(),
  base_portfolio, create = FALSE, assign_to_registry = TRUE
) {
  checkmate::assert_character(short_name)
  checkmate::assert_logical(create)
  env <- registries$portfolios
  if (exists(short_name, envir = env)) return(get(short_name, envir = env))
  if (!create) stop("SMA does not exist and create is set to FALSE")
  checkmate::assert_character(long_name)
  checkmate::assert_numeric(nav)
  lapply(
    positions,
    function(position) checkmate::assert_r6(position, "Position")
  )
  checkmate::assert_character(base_portfolio)
  base_ptfl <- .portfolio(base_portfolio, create = FALSE)
  sma <- SMA$new(
    long_name,
    short_name,
    holdings_url,
    nav,
    positions,
    base_ptfl
  )
  if (assign_to_registry) {
    assign(short_name, sma, envir = env)
  }
  invisible(sma)
}


#' Create or Retrieve an SMA Rule
#'
#' This function creates or retrieves an SMA rule object associated with a
#'  specific SMA. The rule defines certain conditions or thresholds for the SMA
#'  and can be scoped to positions, portfolios, or count.
#'
#' @param sma_name A string representing the name of the SMA.
#'  Must be a valid SMA name.
#' @param rule_name A string representing the name of the rule.
#' @param scope One of "position", "portfolio", or "count" indicating the scope
#'  of the rule.
#' @param bbfields A character vector of Bloomberg fields to be used in the
#'  rule's logic.
#' @param definition A function defining the rule's logic.
#'  Must be a valid function object. May be `NULL` if `scope` is "count".
#' @param max_threshold (Optional) A numeric value specifying the maximum
#'  threshold for the rule. If not provided, defaults to `Inf`.
#' @param min_threshold (Optional) A numeric value specifying the minimum
#'  threshold for the rule. If not provided, defaults to `-Inf`.
#' @param swap_only A logical value indicating whether the rule applies only to
#'  swaps. Defaults to `FALSE`.
#' @param side (Optional) A string indicating the side of the rule.
#'  Valid values are "long", "short", or "gross". Used for position count rules.
#'
#' @return An object of class `SMARule` representing the SMA rule.
#'
#' @details
#' The function first checks if the rule already exists in the `smarules`
#'  registry. If it exists, the existing rule is returned. Otherwise, a new
#'  rule is created and stored in the registry. The rule's scope determines
#'  whether it applies to individual positions, the entire portfolio, or count.
#'
#' @examples
#' # Example usage:
#' my_rule <- .sma_rule(
#'   sma_name = "example_sma",
#'   rule_name = "example_rule",
#'   scope = "position",
#'   definition = function(x) x > 0,
#'   max_threshold = 100,
#'   min_threshold = 10
#' )
#'
#' @seealso [SMARule()]
#'
#' @import checkmate
#' @export
.sma_rule <- function(
  sma_name,
  rule_name,
  scope,
  definition = NULL,
  bbfields = NULL,
  max_threshold = Inf,
  min_threshold = -Inf,
  swap_only = FALSE,
  gross_exposure = FALSE,
  relative_to = "nav",
  divisor = NULL,
  side = NULL
) {
  checkmate::assert_character(sma_name, len = 1)
  sma <- .sma(sma_name, create = FALSE)
  checkmate::assert_r6(sma, "SMA")

  checkmate::assert_character(rule_name, len = 1)
  key_name <- paste0(sma_name, "::", rule_name)
  env <- registries$smarules
  if (exists(key_name, envir = env)) return(get(key_name, envir = env))

  scope_types <- c("position", "portfolio", "count")
  checkmate::assert_choice(scope, scope_types)

  if (scope != "count") checkmate::assert_function(definition)

  checkmate::assert_numeric(max_threshold)
  checkmate::assert_numeric(min_threshold)

  checkmate::assert_choice(
    relative_to, c("nav", "gmv", "long_gmv", "short_gmv")
  )

  if (!is.null(divisor)) checkmate::assert_r6(divisor, "DivisorProvider")

  if (scope == "count") {
    checkmate::assert_choice(side, c("long", "short", "gross"))
  }


  if (scope == "position") {
    smarule <- SMARulePosition$new(
      sma_name = sma_name,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor
    )
  }
  if (scope == "portfolio") {
    smarule <- SMARulePortfolio$new(
      sma_name = sma_name,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor
    )
  }
  if (scope == "count") {
    smarule <- SMARuleCount$new(
      sma_name = sma_name,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor,
      side = side
    )
  }
  assign(key_name, smarule, envir = env)
  invisible(smarule)
}