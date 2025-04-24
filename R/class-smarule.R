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
  public = list(
    #' @param sma_name Character
    #' @param name Character
    #' @param scope Character
    #' @param definition Formula
    #' @param max_threshold numeric
    #' @param min_threshold numeirc
    initialize = function(
      sma_name = NULL,
      name = NULL,
      scope = NULL,
      definition = NULL,
      max_threshold = NULL, min_threshold = NULL
    ) {
      # Check if sma_name is valid
      if (is.null(sma_name) | !is.character(sma_name)) {
        stop("sma_name must be provided as a string")
      }
      if (!exists(sma_name, envir = .portfolio_registry, inherits = FALSE)) {
        stop("SMA not defined")
      }
      # Check if name is valid
      if (is.null(name) | !is.character(name)) {
        stop("name must be provided as a string")
      }
      # Check if scope is valid
      if (is.null(scope) | !is.character(scope)) {
        stop("scope must be provided as a string")
      }
      if (!scope %in% c("position", "portfolio", "all")) {
        stop("scope not valid")
      }
      # Check if definition is valid
      if (!is.function(definition)) {
        stop("definition is not a function")
      }
      # Check if threshold is valid
      if (is.null(max_threshold) | !is.numeric(max_threshold)) {
        stop("max threshold is not valid")
      }
      if (is.null(min_threshold) | !is.numeric(min_threshold)) {
        stop("max threshold is not valid")
      }
      private$id_ <- length(ls(envir = .smarule_registry)) + 1
      private$sma_name_ <- sma_name
      private$name_ <- name
      private$scope_ <- scope
      private$definition_ <- definition
      private$threshold_ <- threshold
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
    get_min_threshold = function() private$min_threshold_
  ),
  private = list(
    sma_name_ = NULL,
    id_ = NULL,
    name_ = NULL,
    scope_ = NULL,
    definition_ = NULL,
    max_threshold_ = NULL,
    min_threshold_ = NULL
  )
)
