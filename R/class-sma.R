#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Sma_Rules.
#'
#' @import R6
#' @import enfusion
#' @import parallel
#' @include create_position.R
#' @include class-portfolio.R
#' @include get_portfolio.R
#' @export
SMA <- R6::R6Class(   #nolint
  "SMA",
  inherit = Portfolio,
  public = list(
    #' @description
    #' Create a new SMA R6 object.
    #' @param long_name Character. SMA Long Name.
    #' @param short_name Character. SMA Short Name.
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param base_portfolio An object representing the base portfolio.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name, short_name, nav, base_portfolio = NULL, positions = NULL
    ) {
      private$id_ <- length(ls(envir = .portfolio_registry)) + 1
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$base_portfolio_ <- base_portfolio
      private$positions_ <- positions
      private$target_positions_ <- positions
      private$sma_rules_ <- list()
      private$replacements_ <- list()
    },

    #' Add SMA Rule
    #' @description Create SMA Rule and Add to SMA
    #' @param rule An object of class SMARule
    add_rule = function(rule) {
      if (is.null(rule)) {
        stop("rule must be supplied")
      }
      if (!inherits(rule, "SMARule")) {
        stop("rule must be of class SMARule")
      }
      private$sma_rules_[[rule$get_name()]] <- rule
      invisible(self)
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
      private$replacements_[[original_security]] <- replacement_security
      invisible(self)
    },

    # Getters ------------------------------------------------------------------
    #' Get Base Portfolio
    #' @description Get the tagret portfolio
    get_base_portfolio = function() private$base_portfolio_,

    #' Get Base Portfolio Position
    #' @description Get a position in the Base portfolio
    #' @param security_id Security ID
    get_base_portfolio_position = function(security_id = NULL) {
      self$get_base_portfolio()$get_position(security_id)
    },

    #' @description Get the SMA Rules
    #' @return A list of SMA rules
    get_sma_rules = function() {
      if (length(private$sma_rules_) == 0) stop("No SMA rules defined")
      private$sma_rules_
    },

    #' @description Get replacement security for a given replaced security
    #' @param replaced_security_id Security ID of the replaced security (in base ptfl) #nolint
    get_replacement_security = function(replaced_security_id = NULL) {
      if (is.null(replaced_security_id)) private$replacements_
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

    #' @description Check SMA rules against the target positions
    check_sma_rules_target = function() {
      lapply(self$get_sma_rules_, function(x) x$check_rule_target())
    },

    #' @description Get Maximum Position Size given all SMA Rules
    #' @param security_id Security ID
    #' @param cl Cluster object for parallel processing (optional)
    get_max_position_rules = function(security_id = NULL, cl = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      if (is.null(cl) || !inherits(cl, "cluster")) {
        return(min(sapply(
          private$sma_rules_,
          \(rule) rule$get_security_max_value(security_id)
        )))
      }
    },

    #' @description Get Minimum Position Size given all SMA Rules
    #' @param security_id Security ID
    #' @param cl Cluster object for parallel processing (optional)
    get_min_position_rules = function(security_id = NULL, cl = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      if (is.null(cl) | !inherits(cl, "cluster")) {
        return(max(sapply(
          private$sma_rules_,
          \(sec, rule) rule$get_security_min_value(sec),
          sec = security_id
        )))
      }
    },

    #' @description Get Swap Flag for a given security
    #' @param security_id Security ID
    get_swap_flag_position_rules = function(security_id = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      any(vapply(private$sma_rules_, \(rule) rule$check_swap_security(security_id), logical(1)))
    },

    #' @description Rebalance SMA Position
    #' @param security_id Security ID
    #' @param cl Cluster object for parallel processing (optional)
    #' @param assign_position Logical. If TRUE, assign the position to the SMA Target Position
    rebalance_position = function(security_id, assign_position = FALSE) {
      if (is.null(security_id)) stop("Security ID must be supplied")

      base_pos <- private$base_portfolio_$get_position(security_id)
      nav_ratio  <- self$get_nav() / private$base_portfolio_$get_nav()

      replacements <- self$get_replacement_security(security_id)

      get_or_create_target <- function(sec) {
        pos <- try(self$get_target_position(sec), silent = TRUE)
        if (inherits(pos, "try-error")) {
          swap_only <- self$get_swap_flag_position_rules(sec)
          pos <- create_position(private$short_name_, sec, 0, swap = swap_only)
        }
        pos
      }

      tgt_pos <- lapply(replacements, get_or_create_target)

      scaled_qty <- base_pos$get_qty() * nav_ratio
      scaled_pos <- create_position(private$short_name_, security_id, scaled_qty)

      for (pos in tgt_pos) {
        sec_id <- pos$get_id()
        existing_qty <- pos$get_qty()
        price <- pos$get_security()$get_price()
        scaled_price <- scaled_pos$get_security()$get_price()
        max_shares <- self$get_max_position_rules(sec_id)
        min_shares <- self$get_min_position_rules(sec_id)
        if (sec_id == security_id) {
          full_trade_qty <- scaled_pos$get_qty()
        } else{
          full_trade_qty <- scaled_pos$get_mkt_val() / price
        }

        target_qty <- pmin(pmax(existing_qty + full_trade_qty, min_shares), max_shares)
        trade_qty <- target_qty - existing_qty
        pos$set_qty(target_qty)

        if (sec_id == security_id) {
          used_qty <- trade_qty
        } else {
          used_qty <- (trade_qty * price) / scaled_price
        }

        scaled_pos$set_qty(scaled_pos$get_qty() - used_qty)
      }

      if (abs(scaled_pos$get_qty()) > .Machine$double.eps ^ 0.5) {
        stop(sprintf(
          "Non-zero residual after rebalancing ‘%s’: %.6f shares remaining",
          security_id, scaled_pos$get_qty()
        ))
      }

      if (assign_position) {
        lapply(tgt_pos, \(p) self$add_target_position(p, overwrite = TRUE))
      }
      tgt_pos
    }
  ),
  private = list(
    base_portfolio_ = NULL,
    sma_rules_ = list(),
    replacements_ = list()
  )
)
