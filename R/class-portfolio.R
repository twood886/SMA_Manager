#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @import enfusion
#' @importFrom dplyr filter
#' @include api-functions.R
#' @include utils.R
#' @export
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
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
    },
    #' @description Get Target Trade Amount
    #' @param security_id Security ID
    #' @return Target Trade Amount
    get_target_trade_qty = function(security_id = NULL) {
      if (!is.null(security_id)) {
        return(private$get_target_trade_qty_security_(security_id))
      }
      all_sec_id <- sapply(private$target_positions_, \(x) x$get_id())
      trade_qty <- lapply(
        all_sec_id,
        \(sec_id) private$get_target_trade_qty_security_(sec_id)
      )
      setNames(trade_qty, all_sec_id)
    }
  ),
  private = list(
    long_name_ = NULL,
    short_name_ = NULL,
    nav_ = NULL,
    positions_ = NULL,
    target_positions_ = NULL,
    get_target_trade_qty_security_ = function(security_id = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")

      tgt_position <- self$get_target_position(security_id)
      if (inherits(tgt_position, "Position")) {
        tgt_qty <- tgt_position$get_qty()
      } else {
        tgt_qty <- 0
      }

      cur_position <- tryCatch(
        {
          self$get_position(security_id)
        },
        error = function(e) {
          NULL
        }
      )
      if (inherits(cur_position, "Position")) {
        cur_qty <- cur_position$get_qty()
      } else {
        cur_qty <- 0
      }

      list(
        "security_id" = security_id,
        "portfolio_id" = self$get_short_name(),
        "amt" = tgt_qty - cur_qty, 
        "swap" = tgt_position$get_swap()
      )
    }
  )
)
