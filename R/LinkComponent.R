#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include generics.R
NULL


setClassUnion("StanModule_or_Function", c("StanModule", "function"))



#' `LinkComponent` Function Arguments
#'
#' This exists just to contain all the common arguments for [`LinkComponent`] methods.
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
#' @param stan (`StanModule` or `function`)\cr Stan code. See Details.
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
#' The `stan` argument can be either a `StanModule` object or a function.
#' If a function is provided, it must take a single argument, a
#' `LongitudinalModel` object, and return a `StanModule` object. This allows for
#' generic functions to be used for links such as `dsld` which allows for each model
#' to provide their own model specific implementation.
#'
#' @family LinkComponent
#' @name LinkComponent-class
#' @exportClass Link
.LinkComponent <- setClass(
    Class = "LinkComponent",
    slots = list(
        "stan" = "StanModule_or_Function",
        "parameters" = "ParameterList",
        "key" = "character"
    )
)


#' @rdname LinkComponent-class
#' @inheritParams stanmodel_arguments
#' @export
LinkComponent <- function(
    stan,
    parameters = ParameterList(),
    key = "",
    ...
) {
    .LinkComponent(
        stan = stan,
        key = key,
        parameters = parameters,
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
#' @param model (`LongitudinalModel`)\cr The longitudinal model.
#'
#' @family LinkComponent
#' @family as.StanModule
#' @export
as.StanModule.LinkComponent <- function(object, model = NULL, ...) {
    if (is(object@stan, "StanModule")) {
        return(object@stan)
    }
    if (is.function(object@stan)) {
        assert_that(
            is(model, "LongitudinalModel"),
            msg = "`model` must be a LongitudinalModel object"
        )
        stan <- object@stan(model)
        assert_that(
            is(stan, "StanModule"),
            msg = "The function must return a StanModule object"
        )
        return(stan)
    }
    stop("Something went wrong")
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


#' @export
as_print_string.LinkComponent <- function(object, ...) {
    as_print_string(object@parameters)
}



#' Standard Links
#'
#' @description
#'
#' These functions enable the inclusion of several common link functions in the survival model of
#' the joint model.
#'
#' Note that the underlying implementation of these links is specific to each longitudinal model.
#'
#' @param prior (`Prior`)\cr The prior to use for the corresponding link coeficient.
#' @name standard-links
NULL


#' @describeIn standard-links Time to growth link
#' @export
link_ttg <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_ttg",
        stan = linkTTG,
        parameters = ParameterList(Parameter(name = "link_ttg", prior = prior, size = 1))
    )
}


#' @describeIn standard-links Derivative of the SLD over time link
#' @export
link_dsld <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_dsld",
        stan = linkDSLD,
        parameters = ParameterList(Parameter(name = "link_dsld", prior = prior, size = 1))
    )
}


#' @describeIn standard-links Current SLD link
#' @export
link_identity <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_identity",
        stan = linkIdentity,
        parameters = ParameterList(Parameter(name = "link_identity", prior = prior, size = 1))
    )
}


#' @describeIn standard-links No link (fit the survival and longitudinal models independently)
#' @export
link_none <- function() {
    Link()
}
