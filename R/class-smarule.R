#' @title SMA Rule (R6 Onject)
#'
#' @description R6 Class that encapsultes rules for SMAs.
#'
#' @import R6
#' @include class-sma.R
#'
#' @export
SMARule <- R6::R6Class( #nolint
  "SMARule",
  private = list(
    sma_name_ = NULL,
    name_ = NULL,
    scope_ = NULL,
    definition_ = NULL,
    max_threshold_ = NULL,
    min_threshold_ = NULL,
    swap_only_ = NULL
  ),
  public = list(
    #' @param sma_name Character
    #' @param name Character
    #' @param scope Character
    #' @param definition Formula
    #' @param max_threshold numeric
    #' @param min_threshold numeirc
    #' @param swap_only logical
    initialize = function(
      sma_name = NULL,
      name = NULL,
      scope = NULL,
      definition = NULL,
      max_threshold = NULL, min_threshold = NULL,
      swap_only = FALSE
    ) {
      private$sma_name_ <- sma_name
      private$name_ <- name
      private$scope_ <- scope
      private$definition_ <- definition
      private$max_threshold_ <- max_threshold
      private$min_threshold_ <- min_threshold
      private$swap_only_ <- swap_only
    },
    #' Get Id
    #' @description Get Id of SMA Rule
    get_id = function() private$id_,

    #' Get Name
    #' @description Get name of SMA Rule
    get_name = function() private$name_,

    #' Get Scope
    #' @description Get the scope of the SMA Rule
    get_scope = function() private$scope_,

    #' Get Definition
    #' @description Get the definition of the SMA Rule
    get_definition = function() private$definition_,

    #' Get Max Threshold
    #' @description Get the threshold of the SMA Rule
    get_max_threshold = function() private$max_threshold_,

    #' Get Min Threshold
    #' @description Get the threshold of the SMA Rule
    get_min_threshold = function() private$min_threshold_,

    #' Get Swap Only Flag
    #' @description Get the swap only flag of the SMA Rule
    get_swap_only = function() private$swap_only_
  )
)
