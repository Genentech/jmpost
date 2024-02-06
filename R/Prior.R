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
#' @slot sample (`function`)\cr See arguments.
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
        "validation" = "list",
        "sample" = "function"
    )
)


#' @param parameters (`list`)\cr the prior distribution parameters.
#' @param repr_model (`string`)\cr the Stan code representation for the model block.
#' @param repr_data (`string`)\cr the Stan code representation for the data block.
#' @param display (`string`)\cr the string to display when object is printed.
#' @param init (`numeric`)\cr the initial value.
#' @param validation (`list`)\cr the prior distribution parameter validation functions. Must have
#' the same names as the `paramaters` slot.
#' @param sample (`function`)\cr a function to sample from the prior distribution.
#' @rdname Prior-class
Prior <- function(
    parameters,
    display,
    repr_model,
    repr_data,
    init,
    validation,
    sample
) {
    .Prior(
        parameters = parameters,
        repr_model = repr_model,
        repr_data = repr_data,
        init = init,
        display = display,
        validation = validation,
        sample = sample
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

    parameters_rounded <- lapply(x@parameters, round, 5)

    do.call(
        glue::glue,
        append(x@display, parameters_rounded)
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
initialValues.Prior <- function(object, ...) {
    getOption("jmpost.prior_shrinkage") * object@init +
        (1 - getOption("jmpost.prior_shrinkage")) * object@sample(1)
}


# Prior-constructors ----

#' Normal Prior Distribution
#'
#' @param mu (`number`)\cr mean.
#' @param sigma (`number`)\cr standard deviation.
#' @family Prior
#' @export
prior_normal <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "normal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ normal(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        init = mu,
        sample = \(n) local_rnorm(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}


#' Standard Normal Prior Distribution
#'
#'
#' @family Prior
#' @export
prior_std_normal <- function() {
    Prior(
        parameters = list(),
        display = "std_normal()",
        repr_model = "{name} ~ std_normal();",
        repr_data = "",
        init = 0,
        sample = \(n) local_rnorm(n),
        validation = list()
    )
}

#' Cauchy Prior Distribution
#'
#' @param mu (`number`)\cr mean.
#' @param sigma (`number`)\cr scale.
#' @family Prior
#'
#' @export
prior_cauchy <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "cauchy(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ cauchy(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        init = mu,
        sample = \(n) local_rcauchy(n, mu, sigma),
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
#' @family Prior
#'
#' @export
prior_gamma <- function(alpha, beta) {
    Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr_model = "{name} ~ gamma(prior_alpha_{name}, prior_beta_{name});",
        display = "gamma(alpha = {alpha}, beta = {beta})",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        init = alpha / beta,
        sample = \(n) local_rgamma(n, shape = alpha, rate = beta),
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
#' @family Prior
#'
#' @export
prior_lognormal <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "lognormal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ lognormal(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        init = exp(mu + (sigma^2) / 2),
        sample = \(n) local_rlnorm(n, mu, sigma),
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
#' @family Prior
#'
#' @export
prior_beta <- function(a, b) {
    Prior(
        parameters = list(a = a, b = b),
        display = "beta(a = {a}, b = {b})",
        repr_model = "{name} ~ beta(prior_a_{name}, prior_b_{name});",
        repr_data = c(
            "real<lower=0> prior_a_{name};",
            "real<lower=0> prior_b_{name};"
        ),
        init = a / (a + b),
        sample = \(n) local_rbeta(n, a, b),
        validation = list(
            a = \(x) x > 0,
            b = \(x) x > 0
        )
    )
}

#' Only Initial Values Specification
#'
#' @param dist (`Prior`)\cr a prior Distribution
#' @family Prior
#' @description
#' This function is used to specify only the initial values for a parameter.
#' This is primarily used for hierarchical parameters whose distributions
#' are fixed within the model and cannot be altered by the user.
#'
prior_init_only <- function(dist) {
    Prior(
        parameters = list(),
        display = "<None>",
        repr_model = "",
        repr_data = "",
        sample = \(n) {
            dist@sample(n)
        },
        init = dist@init,
        validation = list()
    )
}




#' Uniform Prior Distribution
#'
#' @param alpha (`number`)\cr minimum value parameter.
#' @param beta (`number`)\cr maximum value parameter.
#' @family Prior
#'
#' @export
prior_uniform <- function(alpha, beta) {
    assert_that(
        alpha < beta,
        msg = "`alpha`` must be less than `beta`"
    )
    Prior(
        parameters = list(alpha = alpha, beta = beta),
        display = "uniform(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ uniform(prior_alpha_{name}, prior_beta_{name});",
        repr_data = c(
            "real prior_alpha_{name};",
            "real prior_beta_{name};"
        ),
        init = 0.5 * (alpha + beta),
        sample = \(n) local_runif(n, alpha, beta),
        validation = list(
            alpha = is.numeric,
            beta = is.numeric
        )
    )
}


#' Student-t Prior Distribution
#'
#' @param nu (`number`)\cr Degrees of freedom parameter.
#' @param mu (`number`)\cr Location parameter.
#' @param sigma (`number`)\cr Scale parameter.
#' @family Prior
#'
#' @export
prior_student_t <- function(nu, mu, sigma) {
    Prior(
        parameters = list(
            nu = nu,
            mu = mu,
            sigma = sigma
        ),
        display = "student_t(nu = {nu}, mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ student_t(prior_nu_{name}, prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real<lower=0> prior_nu_{name};",
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        init = mu,
        sample = \(n) local_rt(n, nu, mu, sigma),
        validation = list(
            nu = \(x) x > 0,
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}



#' Logistic Prior Distribution
#'
#' @param mu (`number`)\cr Location parameter.
#' @param sigma (`number`)\cr Scale parameter.
#' @family Prior
#'
#' @export
prior_logistic <- function(mu, sigma) {
    Prior(
        parameters = list(
            mu = mu,
            sigma = sigma
        ),
        display = "logistic(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ logistic(prior_mu_{name}, prior_sigma_{name});",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        init = mu,
        sample = \(n) local_rlogis(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}


#' Log-Logistic Prior Distribution
#'
#' @param alpha (`number`)\cr Scale parameter.
#' @param beta (`number`)\cr Shape parameter.
#' @family Prior
#'
#' @export
prior_loglogistic <- function(alpha, beta) {
    Prior(
        parameters = list(
            alpha = alpha,
            beta = beta
        ),
        display = "loglogistic(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ loglogistic(prior_alpha_{name}, prior_beta_{name});",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        init = alpha * pi / (beta * sin(pi / beta)),
        sample = \(n) {
            local_rloglogis(n, alpha, beta)
        },
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        )
    )
}


#' Inverse-Gamma Prior Distribution
#'
#' @param alpha (`number`)\cr Shape parameter.
#' @param beta (`number`)\cr Scale parameter.
#' @family Prior
#'
#' @export
prior_invgamma <- function(alpha, beta) {
    Prior(
        parameters = list(
            alpha = alpha,
            beta = beta
        ),
        display = "inv_gamma(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ inv_gamma(prior_alpha_{name}, prior_beta_{name});",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        init = beta / (alpha - 1),
        sample = \(n) local_rinvgamma(n, alpha, beta),
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        )
    )
}





#' Stub functions for sampling from distributions
#'
#' @description
#' These functions only exist so that they can be mocked during unit
#' tests in order to provide deterministic values. In most cases
#' these are just straight forward pass throughs for the underlying
#' distributions.
#'
#' @param alpha (`number`)\cr Parameter for underlying distribution.
#' @param beta (`number`)\cr Parameter for underlying distribution.
#' @param mu (`number`)\cr Parameter for underlying distribution.
#' @param sigma (`number`)\cr Parameter for underlying distribution.
#' @param nu (`number`)\cr Parameter for underlying distribution.
#' @param ... Pass any additional arguments to the underlying distribution.
#'
#' @importFrom stats rbeta rcauchy rgamma rlnorm rlogis rnorm rt runif
#'
#' @details
#'
#' ## Log-Logistic
#'
#' There is no log-logistic sampling function within base R so it was implemented
#' in terms of sampling from the CDF distribution. Using the Stan parameterisation
#' the CDF is defined as:
#' \deqn{
#' u = F(x) = \frac{1}{1 + (x/ \alpha)^{-\beta}}
#' }
#' The inverse of this function is:
#' \deqn{
#' x = ((u / (1 - u))^(1 / beta)) * alpha
#' }
#'
#' Thus we can sample u from a \eqn{Uni(0, 1)} distribution and then derive x from this.
#'
#' ## Inverse-Gamma
#'
#' The inverse Gamma distribution is defined as 1/Gamma thus we calculate this simply
#' by sampling sampling from the Gamma distribution and then taking the reciprocal.
#'
#' ## Student-t
#'
#' R's sampling functions only produce the standard Student-t distribution so in order
#' to match Stan's implementation we multiply by the scale parameter and add the location
#' parameter. See this \href{https://stats.stackexchange.com/a/623611}{Stack Overflow} post
#' for details
#'
#' @name Local_Sample
#' @keywords internal
NULL

#' @rdname Local_Sample
local_rnorm <- \(...) rnorm(...)

#' @rdname Local_Sample
local_rcauchy <- \(...) rcauchy(...)

#' @rdname Local_Sample
local_rgamma <- \(...) rgamma(...)

#' @rdname Local_Sample
local_rlnorm <- \(...) rlnorm(...)

#' @rdname Local_Sample
local_rbeta <- \(...) rbeta(...)

#' @rdname Local_Sample
local_runif <- \(...) runif(...)

#' @rdname Local_Sample
local_rt <- \(n, nu, mu, sigma) {
    rt(n, nu) * sigma + mu
}

#' @rdname Local_Sample
local_rlogis <- \(...) rlogis(...)

#' @rdname Local_Sample
local_rloglogis <- \(n, alpha, beta) {
    r <- runif(n)
    ((r / (1 - r))^(1 / beta)) * alpha
}

#' @rdname Local_Sample
local_rinvgamma <- \(n, alpha, beta) {
    1 / rgamma(n, alpha, rate = beta)
}
