#' @include generics.R
NULL


#' Standard Links
#'
#' @typed prior: Prior
#'   A [`Prior`] object.
#' @typed model: LongitudinalModel
#'   A [`LongitudinalModel`] object.
#' @param ... Not used.
#'
#' @description
#' These functions are used to enable the use of the corresponding link function between
#' the survival and longitudinal models in a joint model. Note that the exact implementation
#' of the link function is model specific, see
#' \code{vignette("Statistical Specifications", package = "jmpost")} for more details.
#'
#' @name standard-link-user
#'
#' @examples
#' linkNone()
#' linkDSLD()
#' linkTTG(prior_normal(0, 1), model = LongitudinalGSF())
NULL


#' @describeIn standard-link-user No link (fit the survival and longitudinal models independently)
#' @export
#'
#' @returns A `Link` or `LinkComponent` object representing the requested association.
linkNone <- function() {
    Link()
}


#' @describeIn standard-link-user Time to growth link
#' @export
linkTTG <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkTTG", model)
}
#' @export
#'
#' @returns A `PromiseLinkComponent` object.
linkTTG.PromiseLongitudinalModel <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    PromiseLinkComponent(fun = linkTTG, prior = prior, key = "link_ttg")
}
#' @export
#'
#' @returns This method always raises an error for unsupported models.
linkTTG.default <- function(prior, model, ...) {
    stop(sprintf(
        "Method `linkTTG` is not available for `%s`",
        class(model)[[1]]
    ))
}


#' @describeIn standard-link-user Derivative of the SLD over time link
#' @export
linkDSLD <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkDSLD", model)
}
#' @export
#'
#' @returns A `PromiseLinkComponent` object.
linkDSLD.PromiseLongitudinalModel <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    PromiseLinkComponent(fun = linkDSLD, prior = prior, key = "link_dsld")
}
#' @export
#'
#' @returns This method always raises an error for unsupported models.
linkDSLD.default <- function(prior, model, ...) {
    stop(sprintf(
        "Method `linkDSLD` is not available for `%s`",
        class(model)[[1]]
    ))
}


#' @describeIn standard-link-user Current SLD value link
#' @export
linkIdentity <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkIdentity", model)
}
#' @export
#'
#' @returns A `PromiseLinkComponent` object.
linkIdentity.PromiseLongitudinalModel <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    PromiseLinkComponent(
        fun = linkIdentity,
        prior = prior,
        key = "link_identity"
    )
}
#' @export
#'
#' @returns This method always raises an error for unsupported models.
linkIdentity.default <- function(prior, model, ...) {
    stop(sprintf(
        "Method `linkIdentity` is not available for `%s`",
        class(model)[[1]]
    ))
}


#' @describeIn standard-link-user Growth Parameter link
#' @export
linkGrowth <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkGrowth", model)
}
#' @export
#'
#' @returns A `PromiseLinkComponent` object.
linkGrowth.PromiseLongitudinalModel <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    PromiseLinkComponent(fun = linkGrowth, prior = prior, key = "link_growth")
}
#' @export
#'
#' @returns This method always raises an error for unsupported models.
linkGrowth.default <- function(prior, model, ...) {
    stop(sprintf(
        "Method `linkGrowth` is not available for `%s`",
        class(model)[[1]]
    ))
}


#' @describeIn standard-link-user Shrinkage Parameter link
#' @export
linkShrinkage <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkShrinkage", model)
}
#' @export
#'
#' @returns A `PromiseLinkComponent` object.
linkShrinkage.PromiseLongitudinalModel <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    PromiseLinkComponent(
        fun = linkShrinkage,
        prior = prior,
        key = "link_shrinkage"
    )
}
#' @export
#'
#' @returns This method always raises an error for unsupported models.
linkShrinkage.default <- function(prior, model, ...) {
    stop(sprintf(
        "Method `linkShrinkage` is not available for `%s`",
        class(model)[[1]]
    ))
}
