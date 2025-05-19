#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @import enfusion
#' @importFrom dplyr filter
#' @include api-functions.R
#' @include utils.R
#' @include class-tradeconstructor.R
#' @export
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
  private = list(
    long_name_ = NULL,
    short_name_ = NULL,
    nav_ = NULL,
    positions_ = NULL,
    target_positions_ = NULL,
    rules_ = list(),
    replacements_ = list(),
    trade_constructor = NULL
  ),
  public = list(
    #' @description
    #' Create New Portfolio R6 object
    #' @param long_name Portfolio Long Name
    #' @param short_name Portfolio Short Name
    #' @param nav NAV of portfolio
    #' @param positions list of position items
    initialize = function(
      long_name, short_name, nav, positions
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$positions_ <- positions
      private$target_positions_ <- lapply(positions, \(x) x$clone(deep = TRUE))
      private$rules_ <- list()
      private$replacements_ <- list()
      private$trade_constructor <- TradeConstructor$new()
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get Portfolio short name
    get_short_name = function() private$short_name_,
    #' @description Get Fund NAV
    get_nav = function() private$nav_,
    #' @description
    #' Get list of positions in portfolio
    #' @param id Ticker
    get_position = function(id = NULL) {
      positions <- private$positions_
      if (is.null(id)) return(positions)
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) stop("No position in portfolio with id")
      positions[[which(position_ids == id)]]
    },
    #' @description
    #' Get list of target positions in portfolio
    #' @param id Ticker
    get_target_position = function(id = NULL) {
      positions <- private$target_positions_
      if (is.null(id)) return(positions)
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) stop("No target position in portfolio with id")
      positions[[which(position_ids == id)]]
    },
    #' @description Get the Rules
    #' @return A list of rules
    get_rules = function() {
      private$rules_
    },
    #' @description Get the Trade Constructor
    #' @return The portfolio constructor object
    get_trade_constructor = function() {
      private$trade_constructor
    },
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
    #' @description Check SMA rules against the target positions
    check_rules_target = function() {
      lapply(self$get_rules_, function(x) x$check_rule_target())
    },
    #' @description Get Max and Min Value of the security given all SMA Rules
    #' @param security_id Security ID
    get_security_position_limits = function(security_id = NULL) {
      private$trade_constructor$get_security_position_limits(self, security_id)
    },
    #' @description Get Swap Flag for a given security
    #' @param security_id Security ID
    get_swap_flag_position_rules = function(security_id = NULL) {
      private$trade_constructor$get_swap_flag_position_rules(self, security_id)
    },
    #' @description Calculate the trade quantity for a given security
    #' @param security_id Security ID
    #' @param trade_qty Trade quantity (default: 0)
    calc_proposed_trade = function(security_id = NULL, trade_qty = NULL) {
      assert_string(security_id, "security_id")
      assert_numeric(trade_qty, "trade_qty")
      self$get_trade_constructor()$calc_trade_qty(self, security_id, trade_qty)
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
      assert_inherits(position, "Position", "position")
      existing_pos <- tryCatch(
        self$get_position(position$get_id()),
        error = function(e) {
          NULL
        }
      )
      if (!is.null(existing_pos)) {
        if (overwrite) {
          self$remove_position(position$get_id())
          private$positions_ <- c(private$positions_, position)
        }
      } else {
        private$positions_ <- c(private$positions_, position)
      }
      invisible(NULL)
    },
    #' @description
    #' Add New Target Position to Portfolio
    #' @param position Position S6 Object
    #' @param overwrite Logical. Overwrite existing position if TRUE
    add_target_position = function(position, overwrite = FALSE) {
      if (!inherits(position, "Position")) {
        stop("position must be a Position object")
      }
      existing_pos <- tryCatch(
        self$get_target_position(position$get_id()),
        error = function(e) {
          NULL
        }
      )
      if (!is.null(existing_pos)) {
        if (overwrite) {
          self$remove_target_position(position$get_id())
          private$target_positions_ <- c(private$target_positions_, position)
        }
      } else {
        private$target_positions_ <- c(private$target_positions_, position)
      }
      invisible(NULL)
    },
    #' @description
    #' Remove existing Target Position from Portfolio
    #' @param position_id Position ID
    remove_target_position = function(position_id = NULL) {
      if(is.null(position_id)) {
        private$target_positions_ <- list()
        return(invisible(NULL))
      }
      position_ids <- sapply(private$target_positions_, \(x) x$get_id())
      if (position_id %in% position_ids) {
        private$target_positions_ <- private$target_positions_[position_ids != position_id] #nolint
      }
      invisible(self)
    },
    #' Add Rule
    #' @description Create Rule and Add to Portfolio
    #' @param rule An object of class SMARule
    add_rule = function(rule) {
      assert_inherits(rule, "SMARule", "rule")
      private$rules_[[rule$get_name()]] <- rule
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
    }
  )
)
