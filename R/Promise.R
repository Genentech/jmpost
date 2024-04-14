

#' Promise
#'
#' Abstract class for promise objects to inherit off of
.Promise <- setClass("Promise")


#' Promise of a `LongitudinalModel`
#'
#' An object that promises to resolve to a [`LongutidunalModel`] object.
#'
#' @exportClass PromiseLongitudinalModel
.PromiseLongitudinalModel <- setClass(
    "PromiseLongitudinalModel",
    contains = "Promise"
)

#' @rdname PromiseLongitudinalModel-class
#' @export
PromiseLongitudinalModel <- function() {
    .PromiseLongitudinalModel()
}




#' Promise of a `LinkComponent`
#'
#' An object that promises to resolve to a [`LinkComponent`] object.
#' Inheriting from [`Promise`] and [`LinkComponent`].
#'
#' @slot fun (`function`) \cr a function that returns a `LinkComponent`. See details.
#'
#' @details
#'
#' The `fun` slot should be a function of signature `function(prior, model)` and should return
#' a [`LinkComponent`] object. An error will be thrown if the returned [`LinkComponent`] object
#' does not have the same `key` slot value as the original `PromiseLinkComponent`.
#'
#' @exportClass PromiseLinkComponent
.PromiseLinkComponent <- setClass(
    "PromiseLinkComponent",
    contains = c("Promise", "LinkComponent"),
    slots = list(
        fun = "function"
    )
)

#' @rdname PromiseLinkComponent-class
#' @export
PromiseLinkComponent <- function(fun, prior, key) {
    .PromiseLinkComponent(
        fun = fun,
        stan = StanModule(),
        parameters = ParameterList(Parameter(name = key, prior = prior, size = 1)),
        key = key
    )
}


#' @export
as.StanModule.PromiseLinkComponent <- function(object, model, ...) {
    resolved_object <- resolvePromise(object, model = model)
    as.StanModule(resolved_object, ...)
}

#' Resolve a `PromiseLinkComponent`
#'
#' Resolves a [`PromiseLinkComponent`] object to a [`LinkComponent`] object.
#' An error will be thrown if the returned [`LinkComponent`] object
#' does not have the same `key` slot value as the original [`PromiseLinkComponent`].
#'
#' @param object ([`PromiseLinkComponent`]) \cr the promise to resolve
#' @param model ([`LongitudinalModel`]) \cr the model to resolve the promise with
#' @return ([`LinkComponent`]) \cr the resolved `LinkComponent` object
#'
#' @export
resolvePromise.PromiseLinkComponent <- function(object, model, ...) {
    x <- object@fun(
        prior = object@parameters@parameters[[1]]@prior,
        model = model
    )
    assert_that(
        is(x, "LinkComponent"),
        msg = "Resolved `PromiseLinkComponent` did not produce a `LinkComponent` object"
    )
    assert_that(
        names(object) == names(x),
        msg = paste(
            "Resolved `PromiseLinkComponent` did not produce a `LinkComponent` object",
            "with the same key as the promise"
        )
    )
    x
}
