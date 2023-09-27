#' @include StanModule.R
#' @include ParameterList.R
NULL

#' `StanModel` Function Arguments
#'
#' The documentation lists all the conventional arguments for wrappers around
#' [StanModel()].
#'
#' @param stan (`StanModule`)\cr code containing the Stan code specification.
#' @param parameters (`ParameterList`)\cr the parameter specification.
#' @param parameter (`ParameterList`)\cr the (single) parameter specification.
#' @param ... additional arguments for [StanModel()].
#'
#' @name stanmodel_arguments
#' @keywords internal
NULL

# StanModel-class ----


#' Stan Model Object and Constructor Function
#'
#' @param stan (`StanModule`)\cr code containing the Stan code specification.
#' @param parameters (`ParameterList`)\cr the parameter specification.
#'
#' @slot stan (`StanModule`)\cr See Arguements.
#' @slot parameters (`ParameterList`)\cr See Arguements.
#'
#' @export StanModel
#' @exportClass StanModel
#' @family StanModel
.StanModel <- setClass(
    Class = "StanModel",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList"
    )
)

# StanModel-constructor ----

#' @rdname StanModel-class
StanModel <- function(stan, parameters) {
    .StanModel(
        stan = stan,
        parameters = parameters
    )
}

# as.list-StanModel ----

#' `StanModel` -> `list`
#' @description
#' Returns a named list where each element of the list corresponds
#' to a Stan modelling block e.g. `data`, `model`, etc.
#' @param x ([`StanModel`])\cr A Stan Model
#' @param ... Not Used.
#' @family StanModel
#' @export
as.list.StanModel <- function(x, ...) {
    as.list(x@stan)
}

# getParameters-StanModel ----

#' @rdname getParameters
getParameters.StanModel <- function(object) object@parameters
