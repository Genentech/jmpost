#' @include StanModule.R
#' @include ParameterList.R
NULL

# StanModel-class ----

#' `StanModel`
#'
#' @slot stan (`StanModule`)\cr code containing the Stan code specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#'
#' @exportClass StanModel
.StanModel <- setClass(
    Class = "StanModel",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList"
    )
)

# StanModel-constructor ----

#' @rdname StanModel-class
#'
#' @inheritParams stanmodel_arguments
#'
#' @export
StanModel <- function(stan, parameters, ...) {
    .StanModel(
        stan = stan,
        parameters = parameters,
        ...
    )
}

# as.list-StanModel ----

#' @rdname as.list
setMethod(
    f = "as.list",
    signature = c("StanModel"),
    definition = function(x) {
        as.list(x@stan)
    }
)

# getParameters-StanModel ----

#' @rdname getParameters
setMethod(
    f = "getParameters",
    signature = "StanModel",
    definition = function(object) {
        object@parameters
    }
)
