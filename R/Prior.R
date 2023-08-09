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
        "init" = "numeric",
        "validation" = "list"
    )
)


setValidity(
    Class = "Prior",
    method = function(object) {
        for (param in names(object@parameters)) {
            if (!param %in% names(object@validation)) {
                return(sprintf("Parameter `%s` does not have a validation method", param))
            }
            if (!object@validation[[param]](object@parameters[[param]])) {
                return_message <- sprintf(
                    "Invalid value of `%d` for parameter `%s`",
                    object@parameters[[param]],
                    param
                )
                return(return_message)
            }
        }
        return(TRUE)
    }
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
        init = init,
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}

#' Standard Normal Prior Distribution
#'
#' @inheritParams prior_arguments
#'
#' @export
prior_std_normal <- function(init = 0) {
    .Prior(
        parameters = list(),
        repr = "std_normal();",
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
        init = init,
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
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
        init = init,
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        )
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
        init = init,
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
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
        init = init,
        validation = list(
            a = \(x) x > 0,
            b = \(x) x > 0
        )
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
