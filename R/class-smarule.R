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
    bbfields_ = NULL,
    max_threshold_ = NULL,
    min_threshold_ = NULL,
    swap_only_ = NULL,
    gross_exposure_ = NULL,
    divisor_ = NULL,
    get_portfolio = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = FALSE)
    }
  ),
  public = list(
    #' @param sma_name Character
    #' @param name Character
    #' @param scope Character
    #' @param bbfields Character vector
    #' @param definition Formula
    #' @param max_threshold numeric
    #' @param min_threshold numeirc
    #' @param swap_only logical
    #' @param gross_exposure logical
    #' @param relative_to Character
    #' @param divisor DivisorProvider object
    initialize = function(
      sma_name = NULL,
      name = NULL,
      scope = NULL,
      bbfields = NULL,
      definition = NULL,
      max_threshold = NULL, min_threshold = NULL,
      swap_only = FALSE,
      gross_exposure = FALSE,
      relative_to = "nav",
      divisor = NULL
    ) {
      private$sma_name_ <- sma_name
      private$name_ <- name
      private$scope_ <- scope
      private$bbfields_ <- bbfields
      private$definition_ <- definition
      private$max_threshold_ <- max_threshold
      private$min_threshold_ <- min_threshold
      private$swap_only_ <- swap_only
      private$gross_exposure_ <- gross_exposure
      private$relative_to_ <- relative_to
      if (checkmate::check_class(divisor, "DivisorProvider")) {
        private$divisor_ <- divisor
      } else {
        if (relative_to %in% c("nav", "gmv", "long_gmv", "short_gmv")) {
          private$divisor_ <- DivisorProvider$new(relative_to)
        } else {
          private$divisor_ <- DivisorProvider$new("nav")
        }
      }
    },
    #' Get SMA Name
    #' @description Get the name of the SMA
    get_sma_name = function() private$sma_name_,

    #' Get Id
    #' @description Get Id of SMA Rule
    get_id = function() paste0(private$sma_name_, "::", private$name_),

    #' Get Name
    #' @description Get name of SMA Rule
    get_name = function() private$name_,

    #' Get Scope
    #' @description Get the scope of the SMA Rule
    get_scope = function() private$scope_,

    #' Get Bloomberg Fields
    #' @description Get the Bloomberg fields of the SMA Rule
    get_bbfields = function() private$bbfields_,

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
    get_swap_only = function() private$swap_only_,

    #' Get Gross Exposure Flag
    #' @description Get the gross exposure flag of the SMA Rule
    get_gross_exposure = function() private$gross_exposure_,

    #'Get the SMA Object
    #' @description Get the SMA Object that this rule belongs to
    get_sma = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = TRUE)
    },

    #' Get Relative To
    #' @description Get the relative to field of the SMA Rule
    #' @return Character
    get_relative_to = function() private$relative_to_,

    #' Get Divisor
    #' @description Get the DivisorProvider object
    #' @return DivisorProvider object
    get_divisor = function() private$divisor_,

    #' Apply the Rule Definition
    #' @description Apply the rule definition to a set of security IDs
    #' @param security_id Security ID
    apply_rule_definition = function(security_id) {
      exp <- private$definition_(security_id, private$get_portfolio())
      names(exp) <- security_id
      exp
    },

    #' Build Constraints
    #' @description Build any additional constraints for the optimization
    #' @param ctx ModelContext
    build_constraints = function(ctx) list(),

    #' Objective Terms
    #' @description Build any additional objective terms for the optimization
    #' @param ctx ModelContext
    objective_terms = function(ctx) list()
  )
)