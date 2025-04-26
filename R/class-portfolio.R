#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @import enfusion
#' @importFrom dplyr filter
#' @include create_position.R
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
  cloneable = FALSE,
  public = list(
    #' @description
    #' Create New Portfolio R6 object
    #' @param id Portfolio id
    #' @param long_name Portfolio Long Name
    #' @param short_name Portfolio Short Name
    #' @param nav NAV of portfolio
    #' @param positions list of position items
    initialize = function(
      long_name, short_name, nav, positions
    ) {
      private$id_ <- length(ls(envir = .portfolio_registry)) + 1
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$positions_ <- positions
      private$target_positions_ <- lapply(positions, \(x) x$clone(deep = TRUE))
    },

    #' @description Print
    print = function() {
      long_col_width <- max(15, nchar(private$long_name_) + 2)
      short_col_width <- max(12, nchar(private$short_name_) + 2)
      nav_col_width <- 16
      nav_formatted <- formatC(
        private$nav_,
        format = "f",
        big.mark = ",",
        digits = 0
      )

      # Print the headers with dynamic spacing
      cat(sprintf(
        "%-*s %-*s %-*s\n",
        long_col_width, "Long Name",
        short_col_width, "Short Name",
        nav_col_width, "NAV ($)"
      ))
      cat(strrep("-", long_col_width + short_col_width + nav_col_width + 2), "\n") #nolint

      # Print the values with aligned formatting
      cat(sprintf(
        "%-*s %-*s %-*s\n",
        long_col_width, private$long_name_,
        short_col_width, private$short_name_,
        nav_col_width, nav_formatted
      ))
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
      if (is.null(id)) {
        return(positions)
      }
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) {
        stop("No position in portfolio with id")
      }
      return(positions[[which(position_ids == id)]])
    },

    #' @description
    #' Get list of target positions in portfolio
    #' @param id Ticker
    get_target_position = function(id = NULL) {
      positions <- private$target_positions_
      if (is.null(id)) {
        return(positions)
      }
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) {
        stop("No position in portfolio with id")
      }
      return(positions[[which(position_ids == id)]])
    },

    #' @description
    #' Add Position to Portfolio
    #' @param position Position S6 Object
    #' @param overwrite Logical. Overwrite existing position if TRUE
    add_position = function(position, overwrite = FALSE) {
      if (!inherits(position, "Position")) {
        stop("position must be a Position object")
      }
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
    #' Remove existing Position from Portfolio
    #' @param position_id Position ID
    remove_position = function(position_id) {
      position_ids <- sapply(private$positions_, \(x) x$get_id())
      if (position_id %in% position_ids) {
        private$positions_ <- private$positions_[position_ids != position_id] #nolint
      }
    },

    #' @description
    #' Remove existing Target Positiion from Portfolio
    #' @param position_id Position ID
    remove_target_position = function(position_id) {
      position_ids <- sapply(private$target_positions_, \(x) x$get_id())
      if (position_id %in% position_ids) {
        private$target_positions_ <- private$target_positions_[position_ids != position_id] #nolint
      }
    }

  ),
  private = list(
    id_ = NULL,
    long_name_ = NULL,
    short_name_ = NULL,
    nav_ = NULL,
    positions_ = NULL,
    target_positions_ = NULL
  )
)
