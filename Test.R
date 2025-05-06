library(SMAManager)
library(tidyverse)
library(enfusion)
library(Rblpapi)


con <- blpConnect()

ccmf <- create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  enfusion_url = paste0(
    "https://webservices.enfusionsystems.com/mobile/",
    "rest/reportservice/exportReport?",
    "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
    "CCMF+Consolidated+Position+Listing+-+Options.ppr"
  )
)

source("smas/bemap.R")
source("smas/fmap.R")

rebalance_sma(bemap)
rebalance_sma(fmap)

trades <- mget(
  ls(envir = registries$trades, all.names = TRUE),
  envir = registries$trades, 
  inherits = TRUE
)

trade_df <- dplyr::bind_rows(lapply(trades, \(trade) trade$to_df()))
row.names(trade_df) <- NULL

out <- trade_df %>%
  dplyr::group_by(security_id, swap) %>%
  tidyr::pivot_wider(
    names_from = portfolio_short_name,
    values_from = c(shares, allocation_pct)
  ) %>%
  dplyr::mutate(across(contains("shares_"), ~replace_na(.x, 0))) %>%
  dplyr::mutate(`quantity` = rowSums(across(contains("shares_")))) %>%
  dplyr::select(
    security_id,
    swap,
    quantity,
    contains("allocation_pct_"),
  )
write.csv(
  out,
  file = "20250506_sma_trades.csv",
  row.names = FALSE
)

add_trade("et us equity", "ccmf", -1, T, T)






plot_delta_weights <- function(portfolio_name) {

  .get_delta_weights <- function(portfolio) {
    sapply(
      portfolio$get_position(), 
      function(position) {
        setNames(position$get_delta_pct_nav(), position$get_id())
      }
    )
  }

  .to_df <- function(x) {
    data.frame(
      security_id = names(x),
      weight = x,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  base <- .portfolio(portfolio_name, create = FALSE)
  base_name <- base$get_short_name()
  smas <- get_tracking_smas(base)
  if (is.null(smas)) {
    stop("No tracking SMAs found for the given portfolio.")
  }
  sma_names <- unname(sapply(smas, \(x) x$get_short_name()))
  weights_df <- 
    purrr::reduce(
      lapply(smas, \(x) .to_df(.get_delta_weights(x))), 
      \(x, y) dplyr::full_join(x, y, by = "security_id"), 
      .init = .to_df(.get_delta_weights(base))
    ) |>
    setNames(c("security", base_name, sma_names)) |>
    tidyr::pivot_longer(
      cols = all_of(sma_names),
      names_to = "sma",
      values_to = "weight",
    ) %>%
    dplyr::mutate(
      sma = factor(sma, levels = sma_names),
      weight = replace_na(weight, 0)
    )
  
  fig <- plotly::plot_ly(
    data = weights_df,
    x = ~ .data[[base_name]],
    y = ~ weight,
    color = ~ sma,
    type = "scatter",
    hoverinfo = 'text',
    text = ~ paste(
      "Security: ", security,
      "<br>", base_name, ": ", scales::percent(.data[[base_name]], accuracy = 0.01),
      "<br>", sma,": ", scales::percent(weight, accuracy = 0.01)
    )
  ) |>
  plotly::add_lines(
    x = ~ .data[[base_name]],
    y = ~ .data[[base_name]],
    mode    = "lines",
    line    = list(color = "black", width = 1),
    inherit = FALSE,
    showlegend = FALSE,
    hoverinfo = "none"
  ) |>
  plotly::layout(
    xaxis = list(
      title = paste(base_name, "Delta Weights"),
      tickformat = ".0%"
    ),
    yaxis = list(
      title = "SMA Delta Weights",
      tickformat = ".0%",
      scaleanchor = "x", 
      scaleratio = 1
    ),
    legend = list(
      title = list(text = paste(base_name, "Tracking SMAs"))
    )
  )
  fig
}

smas <- get_tracking_smas(ccmf)



ccmf_weight <- 

to_df(ccmf_weight)


position_test <- ccmf$get_pos

securities <- lapply(ccmf$get_position(), function(x) x$get_security()$get_price())


