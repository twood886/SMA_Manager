#' Null/empty coalescing
#' Returns y if x is NULL, length 0, or NA; otherwise returns x.
#' @param x An object to check.
#' @param y A fallback value to return if x is NULL, length 0, or NA.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
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
#' @param sec_id Optional character vector of Security IDs to update. If NULL,
#'  all securities are updated.
#' @return TRUE (invisible)
#' @export
#' @include api-functions.R
#' @include class-security.R
update_bloomberg_fields <- function(sec_id = NULL) {
  rule_names <- ls(get_registries()$smarules)
  rules <- mget(
    rule_names,
    envir = get_registries()$smarules,
    inherits = TRUE
  )
  rules_bbfields_all <- sapply(rules, \(r) r$get_bbfields(), simplify = TRUE)
  rules_bbfields <- unique(unlist(rules_bbfields_all, use.names = FALSE))
  if (is.null(rules_bbfields) || length(rules_bbfields) == 0) {
    return(invisible(TRUE))
  }
  if (is.null(sec_id)) {
    sec_id <- ls(get_registries()$securities)
  }
  invisible(lapply(sec_id, function(id) .security(id)))
  bbdata <- Rblpapi::bdp(sec_id, fields = rules_bbfields)
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

#' Convert Bloomberg Yellow Key to Security ID
#' @param yellow_key Character. Bloomberg Yellow Key (e.g., "AAPL US Equity").
#' @return Character. Corresponding Security ID.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_match
#' @importFrom checkmate assert_character
#' @export
bbid_to_security_id <- function(id) {
  checkmate::assert_character(id)
  fraction_str <- "(?x)(-?\\d+(?:\\.\\d+)?)\\s*<\\s*(\\d+)\\s*/\\s*(\\d+)\\s*>"
  id_clean <- id %>%
    stringr::str_replace_all("<([A-Za-z]+)>", " \\1") %>%
    stringr::str_replace_all(
      fraction_str,
      function(m) {
        parts <- str_match(m, fraction_str)
        before <- as.numeric(parts[2])
        num <- as.numeric(parts[3])
        denom <- as.numeric(parts[4])
        format(before + num / denom, scientific = FALSE, trim = TRUE)
      }
    )
  id_clean
}


#' @title Get Tracking Portfolios
#' @description Get all portfolios that have a specified base portfolio
#' @param base_portfolio Character of base portfolio short name or Portfolio 
#'  object
#' @returns list of portfolios
get_tracking_portfolios <- function(base_portfolio) {
  port_env <- tryCatch(
    registries$portfolios,
    error = function(e) {
      stop("Failed to access portfolio registry: ", conditionMessage(e))
    }
  )
  
  if (is.null(port_env)) return(NULL)
  
  if (checkmate::test_r6(base_portfolio, "Portfolio")) {
    base_portfolio_name <- base_portfolio$get_short_name()
  } else if (checkmate::test_character(base_portfolio, len = 1)) {
    base_portfolio_name <- base_portfolio
  } else {
    checkmate::assert_r6(base_portfolio, "Portfolio")
  }

  checkmate::assert_character(base_portfolio_name, len = 1)

  portfolio_names <- ls(port_env)
  portfolios <- sapply(
    portfolio_names, 
    function(p) get(p, envir = port_env),
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  portfolio_base <- vapply(
    portfolios,
    function(p) {
      tryCatch(
        p$get_base_portfolio()$get_short_name(),
        error = function(e) NA_character_
      )
    },
    character(1)
  )

  portfolios[portfolio_base %in% base_portfolio_name]
}
