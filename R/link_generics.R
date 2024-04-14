#' @include generics.R
NULL



#' Standard Links
#'
#' @param prior ([`Prior`]) \cr A [`Prior`] object.
#' @param model ([`LongitudinalModel`]) \cr A [`LongitudinalModel`] object.
#' @param ... Not used.
#'
#' @description
#' These functions are used to enable the use of the corresponding link function between
#' the survival and longitudinal models in a joint model. Note that the exact implementation
#' of the link function is model specific, see
#' \code{vignette("Statistical Specifications", package = "jmpost")} for more details.
#'
#' @name standard-link-user
NULL




#' @describeIn standard-link-user No link (fit the survival and longitudinal models independently)
#' @export
linkNone <- function() {
    Link()
}


#' @describeIn standard-link-user Time to growth link
#' @export
linkTTG <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkTTG", model)
}
#' @export
linkTTG.PromiseLongitudinalModel <- function(prior = prior_normal(0, 2), model, ...) {
    PromiseLinkComponent(fun = linkTTG, prior = prior, key = "link_ttg")
}
#' @export
linkTTG.default <- function(prior, model, ...) {
    stop(sprintf("Method `linkTTG` is not available for `%s`", class(model)[[1]]))
}




#' @describeIn standard-link-user Derivative of the SLD over time link
#' @export
linkDSLD <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkDSLD", model)
}
#' @export
linkDSLD.PromiseLongitudinalModel <- function(prior = prior_normal(0, 2), model, ...) {
    PromiseLinkComponent(fun = linkDSLD, prior = prior, key = "link_dsld")
}
#' @export
linkDSLD.default <- function(prior, model, ...) {
    stop(sprintf("Method `linkDSLD` is not available for `%s`", class(model)[[1]]))
}




#' @describeIn standard-link-user Current SLD value link
#' @export
linkIdentity <- function(prior, model = PromiseLongitudinalModel(), ...) {
    UseMethod("linkIdentity", model)
}
#' @export
linkIdentity.PromiseLongitudinalModel <- function(prior = prior_normal(0, 2), model, ...) {
    PromiseLinkComponent(fun = linkIdentity, prior = prior, key = "link_identity")
}
#' @export
linkIdentity.default <- function(prior, model, ...) {
    stop(sprintf("Method `linkIdentity` is not available for `%s`", class(model)[[1]]))
}
