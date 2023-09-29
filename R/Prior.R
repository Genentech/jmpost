#' @include generics.R
NULL

#' `Prior` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`Prior`]
#' constructors.
#'
#' @param init (`number`)\cr initial value.
#' @param x ([`Prior`])\cr A Prior Distribution
#' @param object ([`Prior`])\cr A Prior Distribution
#' @param ... Not Used.
#'
#' @name Prior-Shared
#' @keywords internal
NULL

# Prior-class ----

#' Prior Object and Constructor Function
#'
#' Specifies the prior distribution in a Stan Model
#'
#' @param parameters (`list`)\cr the prior distribution parameters.
#' @param repr (`string`)\cr the Stan code regular expression encoding the distribution.
#' @param init (`numeric`)\cr the initial value.
#' @param validation (`list`)\cr the prior distribution parameter validation functions. Must have
#' the same names as the `paramaters` slot.
#'
#' @slot parameters (`list`)\cr See arguments.
#' @slot repr (`string`)\cr See arguments.
#' @slot init (`numeric`)\cr See arguments.
#' @slot validation (`list`)\cr See arguments.
#'
#' @family Prior
#' @export Prior
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

#' @rdname Prior-class
Prior <- function(parameters, repr, init, validation) {
    .Prior(
        parameters = parameters,
        repr = repr,
        init = init,
        validation = validation
    )
}


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




# coerce-Prior,character ----

#' `Prior` -> `character`
#' @description
#' Returns the character representation of the distribution by inserting the
#' distribution parameters into the `x@repr` string
#' @inheritParams Prior-Shared
#' @family Prior
#' @export
as.character.Prior <- function(x, ...) {
    glue::glue(x@repr, .envir = list2env(x@parameters))
}

#' Prior Getter Functions
#' @description
#' Getter functions for the slots of a [`Prior`] object
#' @inheritParams Prior-Shared
#' @family Prior
#' @name Prior-Getter-Methods
NULL



# initialValues-Prior ----

#' @describeIn Prior-Getter-Methods The prior's initial value
#' @export
initialValues.Prior <- function(object) object@init


# Prior-constructors ----

#' Normal Prior Distribution
#'
#' @param mu (`number`)\cr mean.
#' @param sigma (`number`)\cr standard deviation.
#' @inheritParams Prior-Shared
#' @family Prior
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
#' @inheritParams Prior-Shared
#'
#' @family Prior
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
#' @inheritParams Prior-Shared
#' @family Prior
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
#' @inheritParams Prior-Shared
#' @family Prior
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
#' @inheritParams Prior-Shared
#' @family Prior
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


prior_gumbel <- function(mu, beta, init = mu + sigma * 0.577215664901) {
    .Prior(
        parameters = list(mu = mu, beta = beta),
        repr = "gumbel({mu}, {beta});",
        init = init,
        validation = list(
            mu = is.numeric,
            beta = \(x) x > 0
        )
    )
}


#' Beta Prior Distribution
#'
#' @param a (`number`)\cr first parameter.
#' @param b (`number`)\cr second parameter
#' @inheritParams Prior-Shared
#' @family Prior
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
#' @inheritParams Prior-Shared
#' @family Prior
#'
#' @export
prior_none <- function(init = 0.00001) {
    .Prior(
        parameters = list(),
        repr = "",
        init = init
    )
}
