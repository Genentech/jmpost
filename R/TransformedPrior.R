#' @include Prior.R
#' @include PriorTransform.R
NULL

#' Prior on a Transformed Parameter
#'
#' Places a distribution on `g(x)` rather than directly on the declared model
#' parameter `x`, where `g` is a package-defined transformation. The Stan
#' density includes the log absolute Jacobian of `g`. The same scalar
#' distribution is applied independently to each component of a vector.
#'
#' @typed dist: Prior
#'   a scalar distribution prior: normal, standard normal, Cauchy, gamma,
#'   log-normal, beta, uniform, Student-t, logistic, log-logistic or
#'   inverse-gamma.
#'   Constant, flat, initialization-only, vector-specific, horseshoe and already
#'   transformed priors are not supported.
#' @typed transform: PriorTransform
#'   package-defined transformation from the model parameter to the prior
#'   density scale, created by [prior_transform_exp()] or
#'   [prior_transform_square()].
#' @details
#' Equivalently, supply `transform` directly to a scalar distribution helper.
#' The density is proportional to `p(g(x)) * abs(g'(x))`. Distribution support
#' and any existing limits on `dist` are intersected with the transformation's
#' range and mapped back to bounds on `x`. Subsequent [set_limits()] calls use
#' the model parameter scale and retain these support restrictions. Truncation
#' normalisation is evaluated on the density scale.
#'
#' Initial values are drawn on the density scale within the mapped bounds,
#' inverse-transformed and shrunk towards a centre on the model parameter
#' scale using `jmpost.prior_shrinkage`. If the inverse-transformed distribution
#' centre is outside the bounds or non-finite, an interior reference value is
#' used. Initialization uses bounded rejection sampling and reports an error
#' when the admissible region has too little probability to find valid draws.
#'
#' This changes the prior distribution. Predictor parametrizations and Stan
#' affine multipliers serve different purposes and do not supply this Jacobian.
#'
#' @returns A `TransformedPrior` object, inheriting from `Prior`.
#' @family Prior
#' @export
#' @examples
#' prior_normal(60, 60, transform = prior_transform_exp())
#' prior_invgamma(1.5, 0.3, transform = prior_transform_exp(scale = 2))
#' prior_transform(prior_normal(100, 100), prior_transform_square())
prior_transform <- function(dist, transform) {
    assert_that(is(dist, "Prior"), msg = "`dist` must be a Prior object")
    assert_that(
        is(transform, "PriorTransform"),
        msg = "`transform` must be a package-defined PriorTransform object"
    )
    validObject(transform)
    density <- sub("^\\{name\\} ~ ([a-z_]+)\\(.*$", "\\1", dist@repr_model)
    supported <- c(
        "normal", "std_normal", "cauchy", "gamma", "lognormal", "beta",
        "uniform", "student_t", "logistic", "loglogistic", "inv_gamma"
    )
    assert_that(
        !is(dist, "TransformedPrior") && !dist@.is_const &&
            !dist@.allow_vectors && density %in% supported,
        msg = "`dist` must be a supported, untransformed scalar prior"
    )
    support <- switch(
        density,
        gamma = , lognormal = , loglogistic = , inv_gamma = c(0, Inf),
        beta = c(0, 1),
        uniform = c(dist@parameters$alpha, dist@parameters$beta),
        c(-Inf, Inf)
    )
    # Store intrinsic density support separately from later model-scale bounds.
    density_limits <- c(
        max(0, support[1], dist@limits[1]),
        min(support[2], dist@limits[2])
    )
    assert_that(
        density_limits[1] < density_limits[2],
        msg = "The prior support does not overlap the transformation range"
    )
    dist@limits <- density_limits
    dist@.omit_zero_lower_truncation <- density %in%
        c("gamma", "lognormal", "loglogistic", "inv_gamma")
    limits <- .prior_transform_inverse(transform, density_limits)
    object <- .TransformedPrior(
        dist,
        distribution = dist,
        transform = transform,
        limits = limits
    )
    .update_transformed_prior_initialization(object)
}


#' @rdname prior_transform
#' @slot distribution (`Prior`)\cr Distribution on the transformed scale.
#' @slot transform (`PriorTransform`)\cr Transformation to the density scale.
#' @exportClass TransformedPrior
.TransformedPrior <- setClass(
    "TransformedPrior",
    contains = "Prior",
    slots = c(distribution = "Prior", transform = "PriorTransform")
)


.update_transformed_prior_initialization <- function(object) {
    dist <- object@distribution
    transform <- object@transform
    limits <- object@limits
    centre <- dist@centre
    centre <- if (is.finite(centre) && centre > 0) {
        .prior_transform_inverse(transform, centre)
    } else {
        NA_real_
    }
    if (!is.finite(centre) || centre <= limits[1] || centre >= limits[2]) {
        centre <- if (all(is.finite(limits))) {
            limits[1] / 2 + limits[2] / 2
        } else if (is.finite(limits[1])) {
            limits[1] + max(1, abs(limits[1]) * 0.1)
        } else if (is.finite(limits[2])) {
            limits[2] - max(1, abs(limits[2]) * 0.1)
        } else {
            0
        }
    }
    object@centre <- centre
    object@sample <- function(n) {
        density_limits <- .prior_transform_forward(transform, limits)
        values <- numeric()
        for (attempt in seq_len(100)) {
            draws <- dist@sample(max(100, 2 * n))
            draws <- draws[
                is.finite(draws) & draws > density_limits[1] &
                    draws < density_limits[2]
            ]
            values <- c(values, .prior_transform_inverse(transform, draws))
            values <- values[
                is.finite(values) & values > limits[1] & values < limits[2]
            ]
            if (length(values) >= n) return(values[seq_len(n)])
        }
        stop("Unable to generate initial values within transformed bounds")
    }
    object
}


#' @rdname set_limits
#' @export
set_limits.TransformedPrior <- function(object, lower = -Inf, upper = Inf) {
    support <- .prior_transform_inverse(
        object@transform, object@distribution@limits
    )
    object@limits <- c(max(lower, support[1]), min(upper, support[2]))
    validObject(object)
    .update_transformed_prior_initialization(object)
}


#' @rdname as.character.Prior
#' @export
as.character.TransformedPrior <- function(x, ...) {
    transform <- .prior_transform_stan(x@transform, "x")$value
    limits <- render_stan_limits(x@limits)
    paste0(
        as.character(x@distribution), " on ", transform,
        if (nzchar(limits)) paste0("; x", limits) else ""
    )
}


#' @rdname as.StanModule.Prior
#' @export
as.StanModule.TransformedPrior <- function(object, name, size = 1, ...) {
    scalar <- is.numeric(size) && length(size) == 1 && size == 1
    x <- if (scalar) "{name}" else "{name}[prior_i]"
    code <- .prior_transform_stan(object@transform, x)
    dist <- object@distribution
    dist@limits <- .prior_transform_forward(object@transform, object@limits)
    dist@repr_model <- sub("{name}", code$value, dist@repr_model, fixed = TRUE)
    module <- as.StanModule.Prior(dist, name = name, size = size)
    jacobian <- glue::glue(code$log_jacobian, name = name)
    module@model <- paste0(module@model, "\n    target += ", jacobian, ";")
    if (!scalar) {
        module@model <- paste0(
            "    if (", size, " > 0) {\n",
            "        for (prior_i in 1:", size, ") {\n",
            paste0(
                "        ", strsplit(module@model, "\n")[[1]], collapse = "\n"
            ),
            "\n        }\n    }"
        )
    }
    module
}
