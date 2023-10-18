#' @include generics.R
#' @include StanModule.R
NULL

#' `Prior` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`Prior`]
#' constructors.
#'
#' @param init (`number`)\cr initial value.
#' @param x ([`Prior`])\cr a prior Distribution
#' @param object ([`Prior`])\cr a prior Distribution
#' @param name (`character`)\cr the name of the parameter the prior distribution is for
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
#' @slot parameters (`list`)\cr See arguments.
#' @slot repr_model (`string`)\cr See arguments.
#' @slot repr_data (`string`)\cr See arguments.
#' @slot init (`numeric`)\cr See arguments.
#' @slot validation (`list`)\cr See arguments.
#' @slot display (`string`)\cr See arguments.
#'
#' @family Prior-internal
#' @export Prior
#' @exportClass Prior
.Prior <- setClass(
    Class = "Prior",
    slots = c(
        "parameters" = "list",
        "display" = "character",
        "repr_model" = "character",
        "repr_data" = "character",
        "init" = "numeric",
        "validation" = "list"
    )
)


#' @param parameters (`list`)\cr the prior distribution parameters.
#' @param repr_model (`string`)\cr the Stan code representation for the model block.
#' @param repr_data (`string`)\cr the Stan code representation for the data block.
#' @param display (`string`)\cr the string to display when object is printed.
#' @param init (`numeric`)\cr the initial value.
#' @param validation (`list`)\cr the prior distribution parameter validation functions. Must have
#' the same names as the `paramaters` slot.
#' @rdname Prior-class
Prior <- function(parameters, display, repr_model, repr_data, init, validation) {
    .Prior(
        parameters = parameters,
        repr_model = repr_model,
        repr_data = repr_data,
        init = init,
        display = display,
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


#' `Prior` -> `Character`
#'
#' Converts a [`Prior`] object to a character vector
#' @inheritParams Prior-Shared
#' @family Prior-internal
#' @export
as.character.Prior <- function(x, ...) {
    do.call(
        glue::glue,
        append(x@display, x@parameters)
    )
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "Prior",
    definition = function(object) {
        x <- sprintf("\nPrior Object:\n   %s\n\n", as.character(object))
        cat(x)
        return(object)
    }
)


#' `Prior` -> `StanModule`
#'
#' Converts a [`Prior`] object to a [`StanModule`] object
#'
#' @inheritParams Prior-Shared
#'
#' @family Prior-internal
#' @family as.StanModule
#' @export
as.StanModule.Prior <- function(object, name, ...) {
    string <- paste(
        "data {{",
        paste0("    ", object@repr_data, collapse = "\n"),
        "}}",
        "model {{",
        paste0("    ", object@repr_model, collapse = "\n"),
        "}}",
        sep = "\n"
    )
    StanModule(glue::glue(string, name = name))
}


#' `Prior` -> `list`
#'
#' Converts a Prior object to a list of parameter data values
#' for a Stan model.
#'
#' @inheritParams Prior-Shared
#'
#' @family as_stan_list
#' @family Prior-internal
#' @export
as_stan_list.Prior <- function(object, name, ...) {
    vals <- object@parameters
    vals_names <- names(vals)
    if (length(vals_names) >= 1) {
        names(vals) <- paste0("prior_", vals_names, "_", name)
    }
    return(vals)
}



#' Prior Getter Functions
#' @description
#' Getter functions for the slots of a [`Prior`] object
#' @inheritParams Prior-Shared
#' @family Prior-internal
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
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "normal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ normal(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
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
    Prior(
        parameters = list(),
        display = "std_normal()",
        repr_model = "{name} ~ std_normal();",
        repr_data = "",
        init = init,
        validation = list()
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
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "cauchy(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ cauchy(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
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
    Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr_model = "{name} ~ gamma(prior_alpha_{name}, prior_beta_{name});",
        display = "gamma(alpha = {alpha}, beta = {beta})",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
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
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "lognormal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ lognormal(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
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
#' @inheritParams Prior-Shared
#' @family Prior
#'
#' @export
prior_beta <- function(a, b, init = a / (a + b)) {
    Prior(
        parameters = list(a = a, b = b),
        display = "beta(a = {a}, b = {b})",
        repr_model = "{name} ~ beta(prior_a_{name}, prior_b_{name});",
        repr_data = c(
            "real<lower=0> prior_a_{name};",
            "real<lower=0> prior_b_{name};"
        ),
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
    Prior(
        parameters = list(),
        display = "<None>",
        repr_model = "",
        repr_data = "",
        init = init,
        validation = list()
    )
}
