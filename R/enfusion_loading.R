#' @title Check connection to Enfusion
#' @description Checks connection to Enfusion based on whether a users is logged
#'  into Enfusion or not. Returns TRUE if logged in, FALSE if not.
#' @importFrom httr GET
#' @returns Bool
#' @export
check_enfusion_connection <- function() {
  tryCatch(
    {
      httr::GET("http://127.0.0.1:18443/exportReport")
      TRUE
    },
    error = function(cond) FALSE
  )
}

#' @title Download Enfusion Report Using API from Excel Add-In
#' @description This function downloads Enfusion reports using the same API as
#'  the Enfusion Excel Add-In. This cicumvents the need to use the REST API
#'  which is an additional cost the Enfusion License. It requires logging into
#'  the enfusion application which can be accomplished using the launch
#'  enfusion function.
#' @param reportWebServiceURL The Enfusion Report URL.
#'  Same as the one used when downloading reports in Excel.
#' @importFrom httr GET
#' @importFrom readr read_csv
#' @importFrom dplyr if_all
#' @importFrom dplyr everything
#' @examples
#' library(enfusion)
#' enfusion_process <- launch_enfusion("username", "password")
#' reportWebServiceURL <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=test.trb" #nolint
#' get_enfusion_report(reportWebServiceURL, enfusion_process)
#' @export
get_enfusion_report <- function(reportWebServiceURL) { #nolint
  if (!check_enfusion_connection()) {
    stop("Enfusion is not Running")
  }
  # Change Web Service URL from rest API to app
  report_url <- gsub(
    "https://webservices.enfusionsystems.com/mobile/rest/reportservice/",
    "http://127.0.0.1:18443/",
    reportWebServiceURL
  )
  tryCatch({
    suppressMessages(
      raw_data <- readr::read_csv(report_url, show_col_types = FALSE)
    )
  }, error = function(e) {
    stop("No Response from Enfusion")
  })
  raw_data[!is.na(raw_data$Description), ]
}

#' Internal helper: create many holdings from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
#' @export
.bulk_holding_positions <- function(enfusion_report, portfolio_short_name) {
  con <- tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL)
  if (is.null(con)) Rblpapi::blpConnect()

  n <- nrow(enfusion_report)
  if (!n) return(list())

  # ----- Vectorized ID derivation -----
  types <- enfusion_report[["Instrument Type"]]
  ids   <- enfusion_report[["Description"]]
  yk    <- enfusion_report[["BB Yellow Key"]]

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

  # Unfound Ids: Find by CUSIP (one bdp call for all CUSIPs)
  unfound_idx <- which(is.na(ids) | ids == "")
  if (length(unfound_idx) > 0) {
    cusip_vec <- enfusion_report[["CUSIP"]][unfound_idx]

    for (i in seq_along(unfound_idx)) {
      cusip_i <- cusip_vec[i]
      if (!is.na(cusip_i) && nzchar(cusip_i)) {
        lookup_ids <- tryCatch(
          Rblpapi::lookupSecurity(cusip_i)$security,
          error = function(e) NULL
        )
        if (is.null(lookup_ids) || length(lookup_ids) == 0) next

        bb_ids <- bbid_to_security_id(lookup_ids)
        if (length(bb_ids) == 0) next
        cusip_check <- tryCatch(
          Rblpapi::bdp(bb_ids, c("ID032", "DX194")),
          error = function(e) NULL
        )
        if (is.null(cusip_check) || nrow(cusip_check) == 0) next
        hit <- which(cusip_check$ID032 == cusip_i)[1]
        if (!is.na(hit)) {
          id <- cusip_check$DX194[hit]
          if (!is.na(id) && nzchar(id)) {
            ids[unfound_idx[i]] <- id
          }
        }
      }
    }
  }

  ids <- tolower(ids)
  enfusion_report <- enfusion_report[!is.na(ids) & ids != "", , drop = FALSE]
  ids <- ids[!is.na(ids) & ids != ""]

  # ----- Register missing Security objects (single bdp) -----
  env       <- registries$securities
  uniq_ids  <- unique(ids)
  have_mask <- match(uniq_ids, names(env), nomatch = 0L) > 0L
  new_ids   <- uniq_ids[!have_mask]

  if (length(new_ids)) {
    fields  <- c("DX615", "EX028", "PX_LAST")
    data    <- Rblpapi::bdp(new_ids, fields)
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
          price           = px_final[i]
        ),
        envir = env
      )
    }
  }
  # ----- Build holdings (no splits, no per-row bdp) -----
  types <- enfusion_report[["Instrument Type"]]
  qtys <- dplyr::case_when(
    types == "Listed Option" ~ as.double(enfusion_report[["Option Quantity"]]),
    TRUE                     ~ as.double(enfusion_report[["Stock Quantity"]])
  )

  swap_raw  <- as.logical(enfusion_report[["Is Financed"]])
  swaps     <- ifelse(is.na(swap_raw), FALSE, swap_raw)

  custodians         <- as.character(enfusion_report[["Custodian"]])
  broker_custodians  <- as.character(enfusion_report[["Broker Custodian Name"]])
  custodian_acct_ids <- as.character(enfusion_report[["Custodian Acct Id"]])
  custodian_accts    <- as.character(enfusion_report[["Custodian Acct"]])
  trs_custodian_ids  <- as.character(enfusion_report[["TRS Custodian ID"]])
  trs_custodian_names <- as.character(enfusion_report[["TRS Custodian Name"]])

  for (i in seq_along(ids)) {
    .holding(
      portfolio_short_name,
      ids[i], qtys[i], swaps[i],
      custodians[i], broker_custodians[i],
      custodian_acct_ids[i], custodian_accts[i],
      trs_custodian_ids[i], trs_custodian_names[i],
      create = TRUE, assign_to_portfolio = TRUE
    )
  }

  invisible(NULL)
}

#' Create Portfolio from Enfusion
#'
#' @param long_name Character long name
#' @param short_name Character short name
#' @param holdings_url URL to fetch Enfusion report
#' @return Portfolio R6 object
#' @export
create_portfolio_from_enfusion <- function(
  long_name, short_name, holdings_url
) {
  port <- .portfolio(
    short_name,
    long_name,
    holdings_url,
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
#' @param holdings_url URL to fetch Enfusion report
#' @return SMA R6 object
#' @export
create_sma_from_enfusion <- function(
  long_name, short_name, base_portfolio, holdings_url
) {
  sma <- .sma(
    short_name,
    long_name,
    holdings_url,
    nav = 0,
    positions = list(),
    base_portfolio,
    create = TRUE
  )
  sma$update_enfusion()
  invisible(sma)
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
#'  is of the correct class by calling `checkmate::assert_r6`. If the
#'  associated `Security` object is not already registered in the
#'  `registries$securities` environment, it is added using its unique
#'  identifier as the key.
#'
#' @note This function relies on the `registries$securities` environment being
#'  pre-defined and accessible.
#'
#' @examples
#' # Assuming `positions` is a list of Position objects:
#' .register_securities(positions)
#' @include utils.R
#' @include class-position.R
#' @import checkmate
.register_securities <- function(positions) {
  for (pos in positions) {
    checkmate::assert_r6(pos, "Position")
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
  trade_report <- get_enfusion_report(trade_url)
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