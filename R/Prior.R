#' @include generics.R
#' @include StanModule.R
NULL

#' `Prior` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`Prior`]
#' constructors.
#'
#' @typed centre: number
#'   the central point of distribution to shrink sampled values towards
#'   (for most distributions this is the mean or median if the mean is undefined)
#' @typed x: Prior
#'   a prior Distribution
#' @typed object: Prior
#'   a prior Distribution
#' @typed name: character
#'   the name of the parameter the prior distribution is for
#' @typed size: "`numeric` or `character`"
#'   the parameter size.
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
#' @slot repr_parameters (`string`)\cr See arguments.
#' @slot repr_transformed_parameters (`string`)\cr See arguments.
#' @slot repr_generated_quantities (`string`)\cr See arguments.
#' @slot auxiliary_initial_values (`function`)\cr See arguments.
#' @slot auxiliary_size (`function`)\cr See arguments.
#' @slot centre (`numeric`)\cr See arguments.
#' @slot validation (`list`)\cr See arguments.
#' @slot display (`string`)\cr See arguments.
#' @slot sample (`function`)\cr See arguments.
#' @slot limits (`numeric`)\cr See arguments.
#' @slot .omit_zero_lower_truncation (`logical`)\cr See arguments.
#' @slot .allow_vectors (`logical`)\cr See arguments.
#' @slot .is_const (`logical`)\cr See arguments.
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
        "repr_parameters" = "character",
        "repr_transformed_parameters" = "character",
        "repr_generated_quantities" = "character",
        "auxiliary_initial_values" = "function",
        "auxiliary_size" = "function",
        "centre" = "numeric",
        "validation" = "list",
        "sample" = "function",
        "limits" = "numeric",
        ".omit_zero_lower_truncation" = "logical",
        ".allow_vectors" = "logical",
        ".is_const" = "logical"
    )
)


#' @typed parameters: list
#'   the prior distribution parameters.
#' @typed repr_model: string
#'   the Stan code representation for the model block.
#' @typed repr_data: string
#'   the Stan code representation for the data block.
#' @typed repr_parameters: string
#'   the Stan code representation for the parameters block.
#' @typed repr_transformed_parameters: string
#'   the Stan code representation for the transformed parameters block.
#' @typed repr_generated_quantities: string
#'   the Stan code representation for the generated quantities block.
#' @typed auxiliary_initial_values: function
#'   a function that returns initial values for extra Stan parameters
#'   introduced by the prior.
#' @typed auxiliary_size: function
#'   a function that returns sizes for extra Stan parameters introduced
#'   by the prior.
#' @typed display: string
#'   the string to display when object is printed.
#' @typed centre: numeric
#'   the central point of distribution to shrink sampled values towards
#' @typed validation: list
#'   the prior distribution parameter validation functions. Must have
#'   the same names as the `paramaters` slot.
#' @typed sample: function
#'   a function to sample from the prior distribution.
#' @typed limits: numeric
#'   the lower and upper limits for a truncated distribution.
#' @typed .omit_zero_lower_truncation: flag
#'   whether to omit a lower-zero truncation adjustment.
#' @typed .allow_vectors: flag
#'   whether to allow vector parameters.
#' @typed .is_const: flag
#'   whether this prior fixes the parameter at a constant value.
#' @rdname Prior-class
#'
#' @returns A `Prior` object.
#'
#' @examples
#' # Prior objects are normally created with a distribution helper.
#' prior_normal(0, 1)
Prior <- function(
    parameters,
    display,
    repr_model,
    repr_data,
    centre,
    validation,
    sample,
    repr_parameters = "",
    repr_transformed_parameters = "",
    repr_generated_quantities = "",
    auxiliary_initial_values = \(name, size) list(),
    auxiliary_size = \(name, size) list(),
    limits = c(-Inf, Inf),
    .omit_zero_lower_truncation = FALSE,
    .allow_vectors = FALSE,
    .is_const = FALSE
) {
    .Prior(
        parameters = parameters,
        repr_model = repr_model,
        repr_data = repr_data,
        repr_parameters = repr_parameters,
        repr_transformed_parameters = repr_transformed_parameters,
        repr_generated_quantities = repr_generated_quantities,
        auxiliary_initial_values = auxiliary_initial_values,
        auxiliary_size = auxiliary_size,
        centre = centre,
        display = display,
        validation = validation,
        sample = sample,
        limits = limits,
        .omit_zero_lower_truncation = .omit_zero_lower_truncation,
        .allow_vectors = .allow_vectors,
        .is_const = .is_const
    )
}


setValidity(
    Class = "Prior",
    method = function(object) {
        for (param in names(object@parameters)) {
            if (!param %in% names(object@validation)) {
                return(sprintf(
                    "Parameter `%s` does not have a validation method",
                    param
                ))
            }
            if (
                !object@.allow_vectors &&
                    length(object@parameters[[param]]) != 1
            ) {
                return(sprintf("Parameter `%s` must be a single value", param))
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
        if (object@.allow_vectors) {
            # Check that all parameters have either same length or are
            # length 1.
            all_lengths <- sapply(
                object@parameters,
                length
            )
            none_one_lengths <- setdiff(unique(all_lengths), 1)
            if (length(none_one_lengths) > 1) {
                return("All parameters must be the same length or length 1")
            }
        }
        if (length(object@limits) != 2) {
            return("Limits must be a vector of length 2")
        }
        if (object@limits[1] >= object@limits[2]) {
            return("Lower limit must be less than upper limit")
        }
        if (
            length(object@repr_model) != 1 || !is.character(object@repr_model)
        ) {
            return("Model representation must be length 1 string")
        }
        if (
            length(object@repr_parameters) < 1 ||
                !is.character(object@repr_parameters)
        ) {
            return("Parameters representation must be a character vector")
        }
        if (
            length(object@repr_transformed_parameters) < 1 ||
                !is.character(object@repr_transformed_parameters)
        ) {
            return(
                "Transformed parameters representation must be a character vector"
            )
        }
        return(TRUE)
    }
)


#' @rdname set_limits
#' @export
set_limits.Prior <- function(object, lower = -Inf, upper = Inf) {
    object@limits <- c(lower, upper)
    validObject(object)
    return(object)
}


#' `Prior` -> `Character`
#'
#' Converts a [`Prior`] object to a character vector
#' @inheritParams Prior-Shared
#' @family Prior-internal
#' @export
#'
#' @returns A character vector.
as.character.Prior <- function(x, ...) {
    parameters_rounded <- lapply(x@parameters, round, 5)

    display_string <- do.call(
        glue::glue,
        append(x@display, parameters_rounded)
    )
    display_limits <- if (x@.is_const) "" else render_stan_limits(x)
    if (
        display_limits != "" &&
            display_string != "" &&
            display_string != "<None>"
    ) {
        display_string <- paste0(display_string, display_limits)
    }
    return(display_string)
}


#' Creates Stan Syntax for Truncated distributions
#' @description
#' This function creates the Stan syntax for truncated distributions
#' @typed object: Prior | numeric
#'   prior or lower and upper limits for a truncated distribution.
#' @keywords internal
#' @typedreturn character
#'   the Stan syntax for truncated distributions
#'
#' @returns A character vector containing the Stan truncation syntax.
render_stan_limits <- function(object) {
    UseMethod("render_stan_limits")
}

#' @describeIn render_stan_limits method for [Prior] objects.
#' @exportS3Method NULL
render_stan_limits.Prior <- function(object) {
    if (
        object@.omit_zero_lower_truncation &&
            identical(object@limits, c(0, Inf))
    ) {
        return("")
    }
    render_stan_limits(object@limits)
}

#' @describeIn render_stan_limits method for numeric vectors.
#' @exportS3Method NULL
render_stan_limits.numeric <- function(object) {
    limits <- object
    l_bound <- if (limits[[1]] > -Inf) limits[[1]] else ""
    u_bound <- if (limits[[2]] < Inf) limits[[2]] else ""
    string <- ""
    if (l_bound != "" || u_bound != "") {
        string <- glue::glue(
            " T[{l_bound}, {u_bound}]",
            l_bound = l_bound,
            u_bound = u_bound
        )
    }
    return(string)
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
#'
#' @returns A `StanModule` object.
as.StanModule.Prior <- function(object, name, size = 1, ...) {
    stan_repr <- c(
        object@repr_data,
        object@repr_parameters,
        object@repr_transformed_parameters,
        object@repr_model,
        object@repr_generated_quantities
    )
    if (all(nchar(stan_repr) == 0)) {
        return(StanModule())
    }

    indent_stan <- function(x) {
        x <- x[nchar(x) >= 1]
        if (length(x) == 0) {
            return("")
        }
        paste0("    ", x, collapse = "\n")
    }
    trunctation <- if (object@repr_model != "") {
        paste0(render_stan_limits(object), ";")
    } else {
        ""
    }
    string <- paste(
        "data {{",
        indent_stan(object@repr_data),
        "}}",
        "parameters {{",
        indent_stan(object@repr_parameters),
        "}}",
        "transformed parameters {{",
        indent_stan(object@repr_transformed_parameters),
        "}}",
        "model {{",
        indent_stan(paste0(object@repr_model, trunctation)),
        "}}",
        "generated quantities {{",
        indent_stan(object@repr_generated_quantities),
        "}}",
        sep = "\n"
    )
    StanModule(glue::glue(string, name = name, size = size))
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
#'
#' @returns A named `list` suitable for use as Stan data.
as_stan_list.Prior <- function(object, name, ...) {
    vals <- object@parameters
    if (object@.is_const) {
        names(vals) <- paste0("prior_const_", name)
        return(vals)
    }
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
#'
#' @returns The requested prior initial values or auxiliary-parameter information.
initialValues.Prior <- function(object, ...) {
    n_samples <- 100
    centre_value <- object@centre
    sample_values <- object@sample(n_samples)

    is_scalar <- length(centre_value) == 1
    if (is_scalar) {
        assert_that(
            length(sample_values) == n_samples,
            msg = "Sample function must return a vector of length n_samples"
        )

        samples <- getOption("jmpost.prior_shrinkage") *
            object@centre +
            (1 - getOption("jmpost.prior_shrinkage")) * object@sample(n_samples)

        valid_samples <- samples[
            samples >= min(object@limits) & samples <= max(object@limits)
        ]
        assert_that(
            length(valid_samples) >= 1,
            msg = "Unable to generate an initial value that meets the required constraints"
        )
        if (length(valid_samples) == 1) {
            return(valid_samples)
        }
        return(sample(valid_samples, 1))
    } else {
        n_centre_vals <- length(centre_value)
        assert_that(
            ncol(sample_values) == n_centre_vals,
            msg = paste(
                "Sample function must return a matrix with n_samples rows and the same",
                "number of columns as the length of the centre value"
            )
        )
        samples <- getOption("jmpost.prior_shrinkage") *
            matrix(
                centre_value,
                nrow = n_samples,
                ncol = n_centre_vals,
                byrow = TRUE
            ) +
            (1 - getOption("jmpost.prior_shrinkage")) * sample_values
        valid_samples <- samples[
            apply(samples, 1, function(row) {
                all(row >= min(object@limits) & row <= max(object@limits))
            }),
            ,
            drop = FALSE
        ]
        assert_that(
            nrow(valid_samples) >= 1,
            msg = "Unable to generate an initial value that meets the required constraints"
        )
        if (nrow(valid_samples) == 1) {
            return(valid_samples[1, ])
        } else {
            return(valid_samples[sample(nrow(valid_samples), 1), ])
        }
    }
}


#' @describeIn Prior-Getter-Methods The prior's auxiliary initial values
#' @export
auxiliaryInitialValues.Prior <- function(object, name, size, ...) {
    object@auxiliary_initial_values(name = name, size = size)
}


#' @describeIn Prior-Getter-Methods The prior's auxiliary parameter sizes
#' @export
auxiliarySize.Prior <- function(object, name, size, ...) {
    object@auxiliary_size(name = name, size = size)
}


# Prior-constructors ----

#' Normal Prior Distribution
#'
#' @typed mu: number
#'   mean.
#' @typed sigma: number
#'   standard deviation.
#' @family Prior
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_normal(0, 1)
prior_normal <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "normal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ normal(prior_mu_{name}, prior_sigma_{name})",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        centre = mu,
        sample = \(n) local_rnorm(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}


#' Normal Prior for a Vector Distribution
#'
#' @typed mus: numeric
#'   means.
#' @typed sigmas: numeric
#'   standard deviations.
#' @family Prior
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_normal_vector(c(0, 1), c(1, 2))
prior_normal_vector <- function(mus, sigmas) {
    Prior(
        parameters = list(
            mus = mus,
            sigmas = sigmas,
            dim_mus = length(mus),
            dim_sigmas = length(sigmas)
        ),
        display = "normal(mus = [{toString(mus)}], sigmas = [{toString(sigmas)}])",
        repr_model = "{name} ~ normal(prior_mus_{name}, prior_sigmas_{name})",
        repr_data = c(
            "int<lower=1> prior_dim_mus_{name};",
            "int<lower=1> prior_dim_sigmas_{name};",
            "vector[prior_dim_mus_{name}] prior_mus_{name};",
            "vector<lower=0>[prior_dim_sigmas_{name}] prior_sigmas_{name};"
        ),
        centre = mus,
        sample = \(n) local_rnorm_vector(n, mus, sigmas),
        validation = list(
            mus = \(x) all(is.numeric(x)),
            sigmas = \(x) all(x > 0),
            dim_mus = is.count,
            dim_sigmas = is.count
        ),
        .allow_vectors = TRUE
    )
}

#' Regularized Horseshoe Prior for a Vector Distribution
#'
#' @typed df: number
#'   degrees of freedom of the half-Student-t prior for local shrinkage
#'   parameters.
#' @typed df_global: number
#'   degrees of freedom of the half-Student-t prior for the global shrinkage
#'   parameter.
#' @typed df_slab: number
#'   degrees of freedom of the Student-t slab.
#' @typed scale_global: number
#'   scale of the half-Student-t prior for the global shrinkage parameter.
#' @typed scale_slab: number
#'   scale of the Student-t slab.
#' @family Prior
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_horseshoe(df = 1, df_global = 1, df_slab = 4, scale_global = 0.1, scale_slab = 2)
prior_horseshoe <- function(
    df = 1,
    df_global = 1,
    df_slab = 4,
    scale_global = 1,
    scale_slab = 2
) {
    Prior(
        parameters = list(
            df = df,
            df_global = df_global,
            df_slab = df_slab,
            scale_global = scale_global,
            scale_slab = scale_slab
        ),
        display = paste0(
            "horseshoe(df = {df}, df_global = {df_global}, ",
            "df_slab = {df_slab}, scale_global = {scale_global}, ",
            "scale_slab = {scale_slab})"
        ),
        repr_model = paste(
            # Important: T[0, ] for truncating to positive values!
            "prior_local_{name} ~ student_t(prior_df_{name}, 0, 1) T[0, ];",
            "prior_global_{name} ~ student_t(prior_df_global_{name}, 0, prior_scale_global_{name}) T[0, ];",
            "prior_slab_{name} ~ inv_gamma(prior_df_slab_{name} / 2, prior_df_slab_{name} / 2);",
            "{name} ~ normal(rep_vector(0, {size}), prior_scales_{name})",
            sep = "\n    "
        ),
        repr_data = c(
            "real<lower=0> prior_df_{name};",
            "real<lower=0> prior_df_global_{name};",
            "real<lower=0> prior_df_slab_{name};",
            "real<lower=0> prior_scale_global_{name};",
            "real<lower=0> prior_scale_slab_{name};"
        ),
        repr_parameters = c(
            "vector<lower=0>[{size}] prior_local_{name};",
            "real<lower=0> prior_global_{name};",
            "real<lower=0> prior_slab_{name};"
        ),
        repr_transformed_parameters = c(
            "real<lower=0> prior_c2_{name} = square(prior_scale_slab_{name}) * prior_slab_{name};",
            paste(
                "vector<lower=0>[{size}] prior_scales_{name} =",
                "scales_horseshoe(prior_local_{name}, prior_global_{name}, prior_c2_{name});"
            )
        ),
        repr_generated_quantities = c(
            paste(
                "vector<lower=0, upper=1>[{size}] prior_shrinkage_factors_{name} =",
                "shrinkage_horseshoe(prior_local_{name}, prior_global_{name}, prior_c2_{name});"
            )
        ),
        auxiliary_initial_values = \(name, size) {
            local_size <- if (is.numeric(size)) size else 1
            stats::setNames(
                list(
                    abs(local_rt(local_size, df, 0, 1)),
                    abs(local_rt(1, df_global, 0, scale_global)),
                    local_rinvgamma(1, df_slab / 2, df_slab / 2)
                ),
                paste0(c("prior_local_", "prior_global_", "prior_slab_"), name)
            )
        },
        auxiliary_size = \(name, size) {
            stats::setNames(
                list(size, 1, 1),
                paste0(c("prior_local_", "prior_global_", "prior_slab_"), name)
            )
        },
        centre = 0,
        sample = \(n) {
            local_rhorseshoe(
                n = n,
                df = df,
                df_global = df_global,
                df_slab = df_slab,
                scale_global = scale_global,
                scale_slab = scale_slab
            )
        },
        validation = list(
            df = \(x) x > 0,
            df_global = \(x) x > 0,
            df_slab = \(x) x > 0,
            scale_global = \(x) x > 0,
            scale_slab = \(x) x > 0
        )
    )
}


#' Standard Normal Prior Distribution
#'
#'
#' @family Prior
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_std_normal()
prior_std_normal <- function() {
    Prior(
        parameters = list(),
        display = "std_normal()",
        repr_model = "{name} ~ std_normal()",
        repr_data = "",
        centre = 0,
        sample = \(n) local_rnorm(n),
        validation = list()
    )
}

#' Constant Prior Distribution
#'
#' @typed value: number
#'   the fixed parameter value.
#' @family Prior
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_const(0)
prior_const <- function(value) {
    Prior(
        parameters = list(value = value),
        display = "const(value = {value})",
        repr_model = "",
        repr_data = "real prior_const_{name};",
        centre = value,
        sample = \(n) rep(value, n),
        validation = list(
            value = is.numeric
        ),
        .is_const = TRUE
    )
}

#' Cauchy Prior Distribution
#'
#' @typed mu: number
#'   mean.
#' @typed sigma: number
#'   scale.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_cauchy(0, 1)
prior_cauchy <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "cauchy(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ cauchy(prior_mu_{name}, prior_sigma_{name})",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        centre = mu,
        sample = \(n) local_rcauchy(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        ),
        .omit_zero_lower_truncation = TRUE
    )
}


#' Gamma Prior Distribution
#'
#' @typed alpha: number
#'   shape.
#' @typed beta: number
#'   inverse scale.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_gamma(2, 1)
prior_gamma <- function(alpha, beta) {
    Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr_model = "{name} ~ gamma(prior_alpha_{name}, prior_beta_{name})",
        display = "gamma(alpha = {alpha}, beta = {beta})",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        centre = alpha / beta,
        sample = \(n) local_rgamma(n, shape = alpha, rate = beta),
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        ),
        .omit_zero_lower_truncation = TRUE
    )
}

#' Log-Normal Prior Distribution
#'
#' @typed mu: number
#'   mean of the logarithm.
#' @typed sigma: number
#'   standard deviation of the logarithm.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_lognormal(0, 1)
prior_lognormal <- function(mu, sigma) {
    Prior(
        parameters = list(mu = mu, sigma = sigma),
        display = "lognormal(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ lognormal(prior_mu_{name}, prior_sigma_{name})",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        centre = exp(mu + (sigma^2) / 2),
        sample = \(n) local_rlnorm(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        ),
        .omit_zero_lower_truncation = TRUE
    )
}

#' Beta Prior Distribution
#'
#' @typed a: number
#'   first parameter.
#' @typed b: number
#'   second parameter
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_beta(2, 3)
prior_beta <- function(a, b) {
    Prior(
        parameters = list(a = a, b = b),
        display = "beta(a = {a}, b = {b})",
        repr_model = "{name} ~ beta(prior_a_{name}, prior_b_{name})",
        repr_data = c(
            "real<lower=0> prior_a_{name};",
            "real<lower=0> prior_b_{name};"
        ),
        centre = a / (a + b),
        sample = \(n) local_rbeta(n, a, b),
        validation = list(
            a = \(x) x > 0,
            b = \(x) x > 0
        )
    )
}

#' Initial Values Specification
#'
#' @typed dist: Prior
#'   a prior Distribution
#' @family Prior
#' @description
#' This function is used to specify only the initial values for a parameter.
#' This is primarily used for hierarchical parameters whose distributions
#' are fixed within the model and cannot be altered by the user.
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_init_only(prior_normal(0, 1))
prior_init_only <- function(dist) {
    Prior(
        parameters = list(),
        display = "<None>",
        repr_model = "",
        repr_data = "",
        sample = \(n) {
            dist@sample(n)
        },
        centre = dist@centre,
        validation = list(),
        limits = dist@limits,
        .omit_zero_lower_truncation = dist@.omit_zero_lower_truncation
    )
}


#' Uniform Prior Distribution
#'
#' @typed alpha: number
#'   minimum value parameter.
#' @typed beta: number
#'   maximum value parameter.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_uniform(0, 1)
prior_uniform <- function(alpha, beta) {
    assert_that(
        alpha < beta,
        msg = "`alpha`` must be less than `beta`"
    )
    Prior(
        parameters = list(alpha = alpha, beta = beta),
        display = "uniform(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ uniform(prior_alpha_{name}, prior_beta_{name})",
        repr_data = c(
            "real prior_alpha_{name};",
            "real prior_beta_{name};"
        ),
        centre = 0.5 * (alpha + beta),
        sample = \(n) local_runif(n, alpha, beta),
        validation = list(
            alpha = is.numeric,
            beta = is.numeric
        )
    )
}


#' Student-t Prior Distribution
#'
#' @typed nu: number
#'   Degrees of freedom parameter.
#' @typed mu: number
#'   Location parameter.
#' @typed sigma: number
#'   Scale parameter.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_student_t(4, 0, 1)
prior_student_t <- function(nu, mu, sigma) {
    Prior(
        parameters = list(
            nu = nu,
            mu = mu,
            sigma = sigma
        ),
        display = "student_t(nu = {nu}, mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ student_t(prior_nu_{name}, prior_mu_{name}, prior_sigma_{name})",
        repr_data = c(
            "real<lower=0> prior_nu_{name};",
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        centre = mu,
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
#' @typed mu: number
#'   Location parameter.
#' @typed sigma: number
#'   Scale parameter.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_logistic(0, 1)
prior_logistic <- function(mu, sigma) {
    Prior(
        parameters = list(
            mu = mu,
            sigma = sigma
        ),
        display = "logistic(mu = {mu}, sigma = {sigma})",
        repr_model = "{name} ~ logistic(prior_mu_{name}, prior_sigma_{name})",
        repr_data = c(
            "real prior_mu_{name};",
            "real<lower=0> prior_sigma_{name};"
        ),
        centre = mu,
        sample = \(n) local_rlogis(n, mu, sigma),
        validation = list(
            mu = is.numeric,
            sigma = \(x) x > 0
        )
    )
}


#' Log-Logistic Prior Distribution
#'
#' @typed alpha: number
#'   Scale parameter.
#' @typed beta: number
#'   Shape parameter.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_loglogistic(2, 3)
prior_loglogistic <- function(alpha, beta) {
    Prior(
        parameters = list(
            alpha = alpha,
            beta = beta
        ),
        display = "loglogistic(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ loglogistic(prior_alpha_{name}, prior_beta_{name})",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        centre = alpha * pi / (beta * sin(pi / beta)),
        sample = \(n) {
            local_rloglogis(n, alpha, beta)
        },
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        ),
        .omit_zero_lower_truncation = TRUE
    )
}


#' Inverse-Gamma Prior Distribution
#'
#' @typed alpha: number
#'   Shape parameter.
#' @typed beta: number
#'   Scale parameter.
#' @family Prior
#'
#' @export
#'
#' @returns A `Prior` object.
#'
#' @examples
#' prior_invgamma(3, 2)
prior_invgamma <- function(alpha, beta) {
    Prior(
        parameters = list(
            alpha = alpha,
            beta = beta
        ),
        display = "inv_gamma(alpha = {alpha}, beta = {beta})",
        repr_model = "{name} ~ inv_gamma(prior_alpha_{name}, prior_beta_{name})",
        repr_data = c(
            "real<lower=0> prior_alpha_{name};",
            "real<lower=0> prior_beta_{name};"
        ),
        centre = beta / (alpha - 1),
        sample = \(n) local_rinvgamma(n, alpha, beta),
        validation = list(
            alpha = \(x) x > 0,
            beta = \(x) x > 0
        ),
        .omit_zero_lower_truncation = TRUE
    )
}


# nolint start
#
# Developer Notes
#
# The `median.Prior` function is a rough workaround to help generate initial values for
# hierarchical distributions. The original implementation involved sampling initial values
# for the random effects using the medians of the parent distribution e.g.
# ```
# random_effect ~ beta(a_prior@centre,  b_prior@centre)
# ```
# A problem came up though when we implemented support for constrained distributions
# as there was no longer any guarantee that the median/centre of the distribution is
# a valid value e.g.  `a_prior ~ prior_normal(-200, 400)`.
#
# To resolve this issue the `median.Prior` method was created which simply samples
# multiple observations from the constrained distribution and then takes the median
# of those constrained observations; this then ensures that the value being used
# for the parameters is a valid value
#
# nolint end
#' @importFrom stats median
#' @export
#'
#' @returns A single numeric value giving the prior median.
median.Prior <- function(x, na.rm, ...) {
    vals <- replicate(
        n = 500,
        initialValues(x),
        simplify = FALSE
    ) |>
        unlist()
    median(vals)
}


#' Stub functions for sampling from distributions
#'
#' @description
#' These functions only exist so that they can be mocked during unit
#' tests in order to provide deterministic values. In most cases
#' these are just straight forward pass throughs for the underlying
#' distributions.
#'
#' @typed alpha: number
#'   Parameter for underlying distribution.
#' @typed beta: number
#'   Parameter for underlying distribution.
#' @typed mu: number
#'   Parameter for underlying distribution.
#' @typed sigma: number
#'   Parameter for underlying distribution.
#' @typed nu: number
#'   Parameter for underlying distribution.
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
#'
#' @returns A numeric vector of random draws.
local_rnorm <- \(...) rnorm(...)

#' @rdname Local_Sample
local_rnorm_vector <- \(n, mus, sigmas) {
    mapply(local_rnorm, n = n, mean = mus, sd = sigmas)
}

#' @rdname Local_Sample
local_rhorseshoe <- \(n, df, df_global, df_slab, scale_global, scale_slab) {
    # Note: Half-t distribution is here the same as truncated t
    # distribution because the t distribution is symmetric around 0.
    local <- abs(local_rt(n, df, 0, 1))
    global <- abs(local_rt(n, df_global, 0, scale_global))
    slab <- local_rinvgamma(n, df_slab / 2, df_slab / 2)
    c2 <- scale_slab^2 * slab
    scales <- global * sqrt((c2 * local^2) / (c2 + global^2 * local^2))
    local_rnorm(n, mean = 0, sd = scales)
}

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
