#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include generics.R
#' @include Prior.R
NULL




#' `LinkComponent` Function Arguments
#'
#' This exists to contain all the common arguments for [`LinkComponent`] methods.
#'
#' @param stan (`StanModule`)\cr Stan code.
#' @param x ([`LinkComponent`])\cr a link component.
#' @param object ([`LinkComponent`])\cr a link component.
#' @param ... Not Used.
#'
#' @name LinkComponent-Shared
#' @keywords internal
NULL




#' `LinkComponent`
#'
#' @slot stan (`StanModule`)\cr See Arguments.
#' @slot parameters (`ParameterList`)\cr See Arguments.
#' @slot name (`character`)\cr See Arguments.
#'
#' @param stan (`StanModule`)\cr Stan code. See Details.
#' @param parameters (`ParameterList`)\cr The parameter specification.
#' @param key (`character`)\cr Link identifier. See Details.
#'
#' @details
#'
#' This object provides key information needed to construct a link contribution in the
#' survival model based on the parameters of the longitudinal model.
#'
#' Each link component defines a stan function of the longitudinal model parameters which is
#' multiplied by a model coefficient and added to the survival models hazard function.
#'
#' For full details about the specification of a `LinkComponent` please see
#' \code{vignette("extending-jmpost", package = "jmpost")}.
#'
#' @family LinkComponent
#' @name LinkComponent-class
#' @exportClass Link
.LinkComponent <- setClass(
    Class = "LinkComponent",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList",
        "key" = "character"
    )
)


#' @rdname LinkComponent-class
#' @inheritParams stanmodel_arguments
#' @export
LinkComponent <- function(stan, prior, key, ...) {
    .LinkComponent(
        stan = stan,
        key = key,
        parameters = ParameterList(Parameter(name = key, prior = prior, size = 1)),
        ...
    )
}




#' @family LinkComponent
#' @rdname getParameters
#' @export
getParameters.LinkComponent <- function(object, ...) {
    object@parameters
}


#' @family LinkComponent
#' @rdname initialValues
#' @export
initialValues.LinkComponent <- function(object, n_chains, ...) {
    initialValues(object@parameters, n_chains)
}




#' `LinkComponent` -> `StanModule`
#'
#' Converts a [`LinkComponent`] object to a [`StanModule`] object
#'
#' @inheritParams LinkComponent-Shared
#'
#' @family LinkComponent
#' @family as.StanModule
#' @export
as.StanModule.LinkComponent <- function(object, ...) {
    object@stan
}



#' `LinkComponent` -> `list`
#'
#' @inheritParams LinkComponent-Shared
#'
#' @description
#' Returns a named list where each element of the list corresponds
#' to a Stan modelling block e.g. `data`, `model`, etc.
#'
#' @family LinkComponent
#' @export
as.list.LinkComponent <- function(x, ...) {
    stan <- as.StanModule(x, ...)
    as.list(stan)
}

#' @family LinkComponent
#' @export
as_print_string.LinkComponent <- function(object, ...) {
    as_print_string(object@parameters)
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "LinkComponent",
    definition = function(object) {
        cat(
            paste0(
                "\nLinkComponent with parameter:\n    ",
                as_print_string(object), "\n\n"
            )
        )
    }
)

#' @family LinkComponent
#' @export
names.LinkComponent <- function(x, ...) {
    names(x@parameters)
}
