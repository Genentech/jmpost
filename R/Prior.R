#' @include generics.R
NULL

# Prior-class ----

#' `Prior`
#'
#' @slot parameters (`list`)\cr the prior distribution parameters.
#' @slot repr (`string`)\cr the Stan code regular expression encoding the distribution.
#' @slot init (`numeric`)\cr the initial value.
#'
#' @aliases Prior
#' @exportClass Prior
.Prior <- setClass(
    Class = "Prior",
    slots = c(
        "parameters" = "list",
        "repr" = "character",
        "init" = "numeric"
    )
)

# as.character-Prior ----

#' @rdname as.character
setMethod(
    f = "as.character",
    signature = "Prior",
    definition = function(x) {
        as(x, "character")
    }
)

# coerce-Prior,character ----

#' @rdname as.character
#'
#' @name coerce-Prior-character-method
#' @aliases coerce,Prior,character-method
setAs(
    from = "Prior",
    to = "character",
    def = function(from) {
        glue::glue(from@repr, .envir = list2env(from@parameters))
    }
)

# initialValues-Prior ----

#' @rdname initialValues
setMethod(
    f = "initialValues",
    signature = "Prior",
    definition = function(object) object@init
)

# Prior-constructors ----

#' Normal Prior Distribution
#'
#' @param mu (`number`)\cr mean.
#' @param sigma (`number`)\cr standard deviation.
#' @inheritParams prior_arguments
#'
#' @export
prior_normal <- function(mu, sigma, init = mu) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "normal({mu}, {sigma});",
        init = init
    )
}

#' Cauchy Prior Distribution
#'
#' @param mu (`number`)\cr mean.
#' @param sigma (`number`)\cr scale.
#' @inheritParams prior_arguments
#'
#' @export
prior_cauchy <- function(mu, sigma, init = mu) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "cauchy({mu}, {sigma});",
        init = init
    )
}

#' Gamma Prior Distribution
#'
#' @param alpha (`number`)\cr shape.
#' @param beta (`number`)\cr inverse scale.
#' @inheritParams prior_arguments
#'
#' @export
prior_gamma <- function(alpha, beta, init = alpha / beta) {
    .Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr = "gamma({alpha}, {beta});",
        init = init
    )
}

#' Log-Normal Prior Distribution
#'
#' @param mu (`number`)\cr mean of the logarithm.
#' @param sigma (`number`)\cr standard deviation of the logarithm.
#' @inheritParams prior_arguments
#'
#' @export
prior_lognormal <- function(mu, sigma, init = exp(mu + (sigma^2) / 2)) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "lognormal({mu}, {sigma});",
        init = init
    )
}

#' Beta Prior Distribution
#'
#' @param a (`number`)\cr first parameter.
#' @param b (`number`)\cr second parameter
#' @inheritParams prior_arguments
#'
#' @export
prior_beta <- function(a, b, init = a / (a + b)) {
    .Prior(
        parameters = list(a = a, b = b),
        repr = "beta({a}, {b});",
        init = init
    )
}

#' Only Initial Values Specification
#'
#' @inheritParams prior_arguments
#'
#' @export
prior_none <- function(init = 0.00001) {
    .Prior(
        parameters = list(),
        repr = "",
        init = init
    )
}
