#' @title Create Portfolio
#' @description
#' Create an R6 Portfolio object
#' @param long_name portofolio long name
#' @param short_name portfolio short name
#' @param nav Portfolio NAV
#' @param positions List of positions
#' @export
create_portfolio <- function(
  long_name, short_name = NULL, nav = NULL, positions = NULL
) {
  if (is.null(short_name))
    stop("Portfolio Short Name must be supplied", call. = FALSE)
  if (exists(short_name, envir = .portfolio_registry, inherits = FALSE))
    stop("Portfolio already exists", call. = FALSE)
  portfolio <- Portfolio$new(long_name, short_name, nav, positions)
  assign(short_name, portfolio, envir = .portfolio_registry)
  return(portfolio)
}


#' @title Create Portfolio from Enfusion
#' @description
#' Function to create an R6 Portfolio Object
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion Web URL to
#' @include class-portfolio.R
#' @import parallel
#' @return A \code{Portfolio} object.
#' @export
create_portfolio_from_enfusion <- function(long_name, short_name = NULL, enfusion_url) {

  enfusion_rep <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(`Description`)
  )
  nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])

  # parallelize the positions
  nCores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeCluster(nCores - 1)
  parallel::clusterEvalQ(cl,{
    library(Rblpapi)
    library(SMAManager)
    blpConnect()
  })
  short_name <- short_name

  parallel::clusterExport(
    cl,
    varlist = "create_position_from_enfusion",
    envir   = .GlobalEnv
  )

  parallel::clusterExport(
    cl,
    varlist = c("enfusion_rep", "short_name"),
    envir   = environment()
  )
  # Create a postion for each row in the enfusion file
  positions <- parallel::parLapply(
    cl,
    X = seq_len(nrow(enfusion_rep)),
    fun = function(i) {
      create_position_from_enfusion(
        x = enfusion_rep[i, , drop = FALSE],
        portfolio_short_name = short_name
      )
    }
  )
  parallel::stopCluster(cl)

  # Add securities to registry
  securities <- lapply(
    positions,
    \(pos) pos$get_security()
  )

  for (sec in securities) {
    if (!exists(sec$get_id(), envir = .security_registry, inherits = FALSE)) {
      assign(sec$get_id(), sec, envir = .security_registry)
    }
  }
  create_portfolio(long_name, short_name, nav, positions)
}
