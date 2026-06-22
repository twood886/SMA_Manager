library(SMAManager)
files <- yaml::yaml.load_file("portfolios/portfolios.yml", eval.expr = TRUE)
portfolios <- lapply(files, \(file) read_yml_portfolio(file))
names(portfolios) <- vapply(portfolios, \(p) p$get_short_name(), character(1))

update_bloomberg_fields()


compliance <- sapply(
  portfolios,
  function(p) {
    if ("SMA" %in% class(p)) {
      return(p$check_rule_compliance(update_bbfields = FALSE, verbose = FALSE))
    }
    list(pass = TRUE)
  },
  simplify = FALSE
)

compliance_table <- function(portfolios) {
  compliance <- suppressWarnings(sapply(
    portfolios,
    function(p) {
      if ("SMA" %in% class(p)) {
        return(p$check_rule_compliance(update_bbfields = FALSE, verbose = FALSE))
      }
      list(pass = TRUE)
    },
    simplify = FALSE
  ))

  for (i in 1:length(compliance)) {
    cat(rep("_", 80), sep = ""); cat("\n")
    name <- names(compliance)[[i]]
    cat(name); cat(" : ")
    compliant <- compliance[[i]]$pass
    if (isTRUE(compliant)) {
      cat("All Compliant\n")
    }
    if (isFALSE(compliant)) {
      cat("Not Compliant\n")
      issues <- compliance[[i]]$non_compliant
      for (k in 1:length(issues)) {
        cat(paste0("  *", names(issues)[[k]], "\n"))
      }
    }
  }
}

compliance_table(portfolios)

owl <- sapply(
  portfolios,
  \(p) p$get_position("owl us equity")$get_qty()
)

nav <- sapply(
  portfolios,
  \(p) p$get_nav()
)
ccmf_owl_opt <- 1662200 * 12.25 / nav[1]

trade <- nav * ccmf_owl_opt / 1225


test <- function(base_name, security_id, amount, verbose = FALSE) {
  smas <- get_tracking_portfolios(base_portfolio = base_name)
  update_bloomberg_fields()
  trades <- sapply(
    smas,
    \(sma) sma$replicate_trade(security_id, amount, FALSE),
    simplify = FALSE,
    USE.NAMES = TRUE)
  final_shares <- c(
    setNames(amount, base_name),
    vapply(trades, \(t) t[["trade_shares"]], numeric(1))
  )
  if (isFALSE(verbose)) return(final_shares)
  list(final_shares = final_shares, detail = trades)
}
