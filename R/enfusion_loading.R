#' Internal helper: create many positions from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
#' @export
.bulk_security_positions <- function(enfusion_report, portfolio_short_name) {
  con <- tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL)
  if (is.null(con)) Rblpapi::blpConnect()

  n <- nrow(enfusion_report)
  if (!n) return(list())

  # ----- Vectorized ID derivation -----
  types <- enfusion_report[["Instrument Type"]]
  ids   <- enfusion_report[["Description"]]
  yk    <- enfusion_report[["BB Yellow Key Position"]]

  is_eqopt <- types %in% c("Equity", "Listed Option")
  ids[is_eqopt] <- yk[is_eqopt]

  # Bonds: ONE bdp call for all FIGIs
  bond_idx <- which(types == "Bond")
  if (length(bond_idx)) {
    figi <- enfusion_report[["FIGI"]][bond_idx]
    bond_tab <- Rblpapi::bdp(figi, "DX194")
    key_vec  <- if ("security" %in% names(bond_tab)) {
      bond_tab[["security"]]
    } else {
      rownames(bond_tab)
    }
    ids[bond_idx] <- bond_tab$DX194[match(figi, key_vec)]
  }

  ids <- tolower(ids)

  # ----- Register missing Security objects (single bdp) -----
  env       <- registries$securities
  uniq_ids  <- unique(ids)
  have_mask <- match(uniq_ids, names(env), nomatch = 0L) > 0L
  new_ids   <- uniq_ids[!have_mask]

  if (length(new_ids)) {
    fields <- c("DX615", "EX028", "PX_LAST", "BI012", "BI013")
    data   <- Rblpapi::bdp(new_ids, fields)
    data_key <- if ("security" %in% names(data)) {
      data[["security"]]
    } else {
      rownames(data)
    }
    pos <- match(new_ids, data_key)

    inst_type <- data$EX028[pos]
    px_last   <- data$PX_LAST[pos]
    px_final  <- ifelse(inst_type == "FixedIncome", px_last / 100, px_last)

    for (i in seq_along(new_ids)) {
      bbid <- new_ids[i]
      if (exists(bbid, envir = env, inherits = FALSE)) next
      assign(
        bbid,
        Security$new(
          bbid            = bbid,
          description     = data$DX615[pos[i]],
          instrument_type = inst_type[i],
          price           = px_final[i],
          bics_level_2    = data$BI012[pos[i]],
          bics_level_3    = data$BI013[pos[i]]
        ),
        envir = env
      )
    }
  }

  # ----- Build positions (no splits, no per-row bdp) -----
  qty <- ifelse(
    types == "Listed Option",
    as.double(enfusion_report[["Option Quantity"]]),
    as.double(enfusion_report[["Stock Quantity"]])
  )
  swap_raw <- as.logical(enfusion_report[["Is Financed"]])
  swap     <- ifelse(is.na(swap_raw), FALSE, swap_raw)

  positions <- Map(
    \(id, q, s) .position(portfolio_short_name, id, q, swap = s), ids, qty, swap
  )
  positions <- unname(positions)
  positions
}



#' Create Portfolio from Enfusion
#'
#' @param long_name Character long name
#' @param short_name Character short name
#' @param enfusion_url URL to fetch Enfusion report
#' @return Portfolio R6 object
#' @export
create_portfolio_from_enfusion <- function(
  long_name, short_name, holdings_url, trade_url
) {
  port <- .portfolio(
    short_name,
    long_name,
    holdings_url,
    trade_url,
    nav = 0,
    positions = list(),
    create = TRUE
  )
  port$update_enfusion()
  invisible(port)
}

#' Create SMA from Enfusion
#'
#' @param long_name Character long name
#' @param short_name Character short name
#' @param base_portfolio Portfolio object
#' @param enfusion_url URL to fetch Enfusion report
#' @return SMA R6 object
#' @export
create_sma_from_enfusion <- function(
  long_name, short_name, base_portfolio, holdings_url, trade_url
) {
  enfusion_report <- dplyr::filter(
    enfusion::get_enfusion_report(holdings_url),
    !is.na(.data$Description) #nolint
  )
  nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])
  if (is.na(nav)) {
    nav <- 0
  }
  positions <- .bulk_security_positions(
    enfusion_report = enfusion_report,
    portfolio_short_name = short_name
  )
  .sma(
    short_name,
    long_name,
    holdings_url,
    trade_url,
    nav,
    positions,
    base_portfolio,
    create = TRUE
  )
}


#' Register Securities in the Securities Registry
#'
#' This function iterates over a list of `Position` objects, validates their
#'  type and registers their associated `Security` objects in the securities
#'  registry if they are not already present.
#'
#' @param positions A list of `Position` objects. Each `Position` object must
#'  have a method `get_security()` that returns a `Security` object, and each
#' `Security` object must have a method `get_id()` that returns a unique
#'  identifier.
#'
#' @return Returns `NULL` invisibly.
#'
#' @details The function ensures that each `Position` object in the input list
#'  is of the correct class by calling `SMAManager:::assert_inherits`. If the
#'  associated `Security` object is not already registered in the
#'  `registries$securities` environment, it is added using its unique
#'  identifier as the key.
#'
#' @note This function relies on the `registries$securities` environment being
#'  pre-defined and accessible. It also assumes that the `SMAManager` package
#'  provides the `assert_inherits` function for type validation.
#'
#' @examples
#' # Assuming `positions` is a list of Position objects:
#' .register_securities(positions)
.register_securities <- function(positions) {
  for (pos in positions) {
    SMAManager:::assert_inherits(pos, "Position", "pos")
    sec <- pos$get_security()
    if (!exists(sec$get_id(), envir = registries$securities)) {
      assign(sec$get_id(), sec, envir = registries$securities)
    }
  }
  invisible(NULL)
}

#' Internal helper: create many positions from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
#' @export
.bulk_trade_positions <- function(trade_url, portfolio) {
  # Ensure Bloomberg session
  con <- tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL)
  if (is.null(con)) Rblpapi::blpConnect()

  # Pull report + keep only rows we need
  trade_report <- enfusion::get_enfusion_report(trade_url)
  if (is.null(trade_report) || !nrow(trade_report)) return(invisible(NULL))

  keep <- !is.na(trade_report[["Description"]]) &
    (trade_report[["Order Remaining Quantity"]] != 0)
  if (!any(keep)) return(invisible(NULL))
  tr <- trade_report[keep, , drop = FALSE]

  # ----- Vectorized ID derivation -----
  types <- tr[["Instrument Type"]]
  ids   <- tr[["Description"]]                      # default to Description
  yk    <- tr[["BB Yellow Key"]] %||% tr[["BB Yellow Key Position"]]

  is_eqopt <- types %in% c("Equity", "Listed Option")
  ids[is_eqopt] <- yk[is_eqopt]

  # One Bloomberg bdp() for all bonds
  bond_idx <- which(types == "Bond")
  if (length(bond_idx)) {
    figi <- tr[["FIGI"]][bond_idx]
    bond_tab <- Rblpapi::bdp(figi, "DX194")
    key <- if ("security" %in% names(bond_tab)) {
      bond_tab[["security"]]
    } else {
      rownames(bond_tab)
    }
    ids[bond_idx] <- bond_tab$DX194[match(figi, key)]
  }

  ids <- tolower(ids)

  # ----- Vectorized quantities & flags -----
  remain <- as.double(tr[["Order Remaining Quantity"]])
  total  <- as.double(tr[["Notional Quantity"]])
  # Match your sign flip rule: if total < 0 => negate remain
  remain <- remain * ifelse(is.na(total), 1, ifelse(total < 0, -1, 1))

  swap_raw <- as.logical(tr[["Is Financed"]])
  swap     <- ifelse(is.na(swap_raw), FALSE, swap_raw)

  # ----- Execute trades -----
  # (.trade likely has side effects and cannot be vectorized safely)
  port_id <- portfolio$get_short_name()
  mapply(
    function(id, q, s) {
      .trade(
        security_id         = id,
        portfolio_id        = port_id,
        qty                 = q,
        swap                = s,
        create              = TRUE,
        assign_to_registry  = TRUE
      )
      NULL
    },
    id = ids, q = remain, s = swap,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  invisible(NULL)
}