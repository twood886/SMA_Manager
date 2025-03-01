#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @import enfusion
#' @importFrom dplyr filter
#' @include create_position.R
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
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
      private$target_positions_ <- positions
    },

    #' @description Print
    print = function() {
      long_col_width <- max(15, nchar(private$long_name_) + 2)
      short_col_width <- max(12, nchar(private$short_name_) + 2)
      nav_col_width <- 16
      nav_formatted <- formatC(private$nav_, format = "f", big.mark = ",", digits = 0) #nolint

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