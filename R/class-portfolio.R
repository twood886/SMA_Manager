#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @importFrom dplyr filter
#' @include api-functions.R
#' @include utils.R
#' @include class-tradeconstructor.R
#' @include enfusion_loading.R
#' @export
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
  private = list(
    long_name_ = NULL,
    short_name_ = NULL,
    nav_ = NULL,
    positions_ = NULL,
    rules_ = list(),
    replacements_ = list(),
    trade_constructor = NULL,
    holdings_url_ = NULL,
    trade_url_ = NULL
  ),
  public = list(
    #' @description
    #' Create New Portfolio R6 object
    #' @param long_name Portfolio Long Name
    #' @param short_name Portfolio Short Name
    #' @param nav NAV of portfolio
    #' @param positions list of position items
    #' @param holdings_url URL to Enfusion Holdings Report
    initialize = function(
      long_name, short_name, holdings_url, nav, positions
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$positions_ <- positions
      private$rules_ <- list()
      private$replacements_ <- list()
      private$trade_constructor <- TradeConstructor$new()
      private$holdings_url_ <- holdings_url
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get Portfolio short name
    get_short_name = function() private$short_name_,
    #' @description Get Fund NAV
    get_nav = function() private$nav_,
    #' @description Get list of positions in portfolio
    #' @param id Ticker
    get_position = function(id = NULL) {
      positions <- private$positions_
      if (is.null(id)) return(positions)
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) stop("No position in portfolio with id")
      positions[[which(position_ids == id)]]
    },
    #' @description Get the Rules
    #' @return A list of rules
    get_rules = function() private$rules_,
    #' @description Get the Trade Constructor
    #' @return The portfolio constructor object
    get_trade_constructor = function() private$trade_constructor,
    #' @description Get replacement security for a given replaced security
    #' @param replaced_security_id Security ID of the replaced security (in base ptfl) #nolint
    get_replacement_security = function(replaced_security_id = NULL) {
      if (is.null(replaced_security_id)) return(private$replacements_)
      if (!replaced_security_id %in% names(private$replacements_)) {
        return(replaced_security_id)
      }
      private$replacements_[[replaced_security_id]]
    },
    #' @description Get replaced security for a given replacement security
    #' @param replacement_security_id Security ID of the replacement security (in SMA) #nolint
    get_replaced_security = function(replacement_security_id = NULL) {
      if (is.null(replacement_security_id)) names(private$replacements_)
      u <- unlist(private$replacements_, use.names = TRUE)
      idx <- which(u == replacement_security_id)
      if (length(idx) == 0) return(NULL)
      names(u)[idx]
    },
    #' @description Get Max and Min Value of the security given all SMA Rules
    #' @param security_id Security ID
    #' @param update_bbfields Logical. Update Bloomberg fields (default: TRUE) #nolint
    get_security_position_limits = function(
      security_id = NULL,
      update_bbfields = TRUE
    ) {
      checkmate::assert_flag(update_bbfields)
      if (update_bbfields) {
        update_bloomberg_fields()
      }
      private$trade_constructor$get_security_position_limits(self, security_id)
    },
    #' @description Get Swap Flag for a given security
    #' @param security_id Security ID
    #' @param update_bbfields Logical. Update Bloomberg fields (default: TRUE) #nolint
    get_swap_flag_position_rules = function(
      security_id = NULL,
      update_bbfields = TRUE
    ) {
      checkmate::assert_flag(update_bbfields)
      if (update_bbfields) {
        update_bloomberg_fields()
      }
      private$trade_constructor$get_swap_flag_position_rules(self, security_id)
    },
    # Setter Functions ---------------------------------------------------------
    #' @description
    #' Add flow to portfolio
    #' @param flow flow amount
    add_flow = function(flow = 0) {
      private$nav_ <- private$nav_ + flow
      invisible(NULL)
    },
    #' @description
    #' Add Position to Portfolio
    #' @param position Position S6 Object
    #' @param overwrite Logical. Overwrite existing position if TRUE
    add_position = function(position, overwrite = FALSE) {
      checkmate::assert_r6(position, "Position")
      position_ids <- sapply(self$get_position(), \(x) x$get_id())
      existing_pos <- position$get_id() %in% position_ids
      if (isTRUE(existing_pos)) {
        if (overwrite) {
          other_positions <- self$get_position()[
            which(position_ids != position$get_id())
          ]
          private$positions_ <- c(other_positions, position)
        }
      } else {
        private$positions_ <- c(private$positions_, position)
      }
      invisible(NULL)
    },
    #' Add Rule
    #' @description Create Rule and Add to Portfolio
    #' @param rule An object of class SMARule
    add_rule = function(rule) {
      checkmate::assert_r6(rule, "SMARule")
      private$rules_[[rule$get_name()]] <- rule
      invisible(rule)
    },
    #' Add Replacement
    #' @description Add replacement securitity
    #' @param original_security The original Security id
    #' @param replacement_security The replacement Security id
    add_replacement = function(
      original_security = NULL, replacement_security = NULL
    ) {
      if (is.null(original_security) | is.null(replacement_security)) {
        stop("Securities must be provided")
      }
      original_security <- tolower(original_security)
      replacement_security <- tolower(replacement_security)
      .security(replacement_security)
      private$replacements_[[original_security]] <- replacement_security
      invisible(NULL)
    },
    # Updaters -----------------------------------------------------------------
    #' Update Enfusion Data
    #' @description Update Positions
    update_enfusion = function() {
      enfusion_report <- dplyr::filter(
        get_enfusion_report(private$holdings_url_),
        !is.na(.data$Description) & .data$`Instrument Type` != "Cash"
      )
      nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])
      if (is.na(nav)) nav <- 0
      private$nav_ <- nav
      .bulk_holding_positions(enfusion_report, private$short_name_)
      invisible(self)
    },
    # Calculators --------------------------------------------------------------
    #' Rebalance Portfolio
    #' @description Calculate the trade quantity for a given security
    #' @param update_bbfields Logical. Update Bloomberg fields (default: TRUE)
    #' @param as.df Logical. Return as data frame (default: TRUE)
    rebalance = function(update_bbfields = TRUE, as.df = TRUE) {
      checkmate::assert_flag(update_bbfields)
      checkmate::assert_flag(as.df)
      if (update_bbfields) update_bloomberg_fields()
      rebal <- self$get_trade_constructor()$optimize_sma(self)
      current_sh <- vapply(self$get_position(), \(p) p$get_qty(), numeric(1))
      current_ids <- vapply(self$get_position(), \(p) p$get_id(), character(1))
      names(current_sh) <- current_ids
      sec_ids <- unique(c(names(rebal$target_weights), current_ids))

      if (!as.df) return(rebal)
      data.frame(
        security_id   = sec_ids,
        target_weights = tidyr::replace_na(rebal$target_weights[sec_ids], 0),
        final_weights = tidyr::replace_na(rebal$weights[sec_ids], 0),
        final_shares  = tidyr::replace_na(rebal$shares[sec_ids], 0),
        current_shares = tidyr::replace_na(current_sh[sec_ids], 0),
        trade = tidyr::replace_na(rebal$shares[sec_ids], 0) -
          tidyr::replace_na(current_sh[sec_ids], 0),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  )
)