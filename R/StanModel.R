#' @include StanModule.R
#' @include ParameterList.R
#' @include generics.R
NULL

#' `StanModel` Function Arguments
#'
#' The documentation lists all the conventional arguments for wrappers around
#' [StanModel()].
#'
#' @typed stan: StanModule
#'   code containing the Stan code specification.
#' @typed parameters: ParameterList
#'   the parameter specification.
#' @typed parameter: ParameterList
#'   the (single) parameter specification.
#' @typed name: character
#'   display name for the model object.
#' @param ... additional arguments for [StanModel()].
#'
#' @name stanmodel_arguments
#' @keywords internal
NULL

# StanModel-class ----

#' Stan Model Object and Constructor Function
#'
#' @slot stan (`StanModule`)\cr See Arguments.
#' @slot parameters (`ParameterList`)\cr See Arguments.
#' @slot name (`character`)\cr display name for the model object.
#'
#' @export StanModel
#' @exportClass StanModel
#' @family StanModel
.StanModel <- setClass(
    Class = "StanModel",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList",
        "name" = "character"
    )
)

# StanModel-constructor ----

#' @inheritParams stanmodel_arguments
#' @rdname StanModel-class
StanModel <- function(stan, parameters, name = "<Unnamed>") {
    .StanModel(
        stan = stan,
        parameters = parameters,
        name = name
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
#' @export
getParameters.StanModel <- function(object, ...) object@parameters


#' @export
as_print_string.StanModel <- function(object, ...) {
    string <- sprintf(
        "\n%s Model Object with parameters:\n%s\n\n",
        object@name,
        paste("   ", as_print_string(object@parameters)) |>
            paste(collapse = "\n")
    )
    return(string)
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "StanModel",
    definition = function(object) {
        cat(as_print_string(object))
    }
)

#' @rdname initialValues
#' @export
initialValues.StanModel <- function(object, n_chains, ...) {
    initialValues(object@parameters, n_chains)
}
