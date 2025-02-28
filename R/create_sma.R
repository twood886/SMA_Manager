#' @title Create SMA
#'
#' @description
#' Fucntion to create an R6 SMA Object.
#'
#' @param long_name Long name of SMA object.
#' @param short_name Short identifying name of SMA object.
#' @param target_portfolio Short name of portfolio the SMA is
#'  intended to mimick. This portfolio must already be created for
#'  the SMA to be created.
#' @param enfusion_url Optional Web URL to Enfusion
#'  Consolidated Position Listing Report. If supplied, current
#'  positions in the SMA will be created based on the holdings
#'  in the enfusion report. If not supplied, NAV must be supplied.
#' @param nav Optional NAV of SMA. If enfusion_url is supplied, arguement will
#'  be ignored. If enfusion_url is not supplied, target_positions will be based
#'  on the target portfolio allocations, scaled to the NAV specified.
#'
#' @return A \code{SMA} object.
#' @export
#' @include get_portfolio.R
#' @include class-sma.R
create_sma <- function(
  long_name, short_name, target_portfolio = NULL,
  enfusion_url = NULL, nav = NULL
) {

  # Check that target portfolio is supplied
  if (is.null(target_portfolio)) {
    stop("Target Portfolio Must be Supplied")
  }
  # Check that target portfolio has already been created
  tgt_ptfl <- get_portfolio(target_portfolio)
  if (is.null(tgt_ptfl)) {
    stop("Target Portfolio has not been created")
  }

  # Check enfusion url or NAV is supplied
  # If enfusion url is provided create the real SMA
  # If enfusion url is not provided create a fake SMA using NAV
  if (is.null(enfusion_url) && is.null(nav)) {
    stop("Need to provide the URL to an existing SMA or NAV to create test")
  }

  # If enfusion url is provided download report and create positions
  if (!is.null(enfusion_url)) {
    enfusion_rep <- dplyr::filter(
      enfusion::get_enfusion_report(enfusion_url),
      !is.na(`Description`)
    )
    nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])
    positions <- apply(enfusion_rep, 1, create_position_from_enfusion)

    sma <- SMA$new(
      long_name = long_name,
      short_name = short_name,
      nav = nav,
      target_portfolio = tgt_ptfl,
      positions = positions
    )
  } else {
    sma <- SMA$new(
      long_name = long_name,
      short_name = short_name,
      nav = nav,
      target_portfolio = tgt_ptfl
    )
  }
  assign(short_name, sma, envir = .sma_registry)
  return(sma)
}
