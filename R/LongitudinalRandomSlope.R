#' @include LongitudinalModel.R
#' @include Link.R
NULL

# LongitudinalRandomSlope-class ----

#' `LongitudinalRandomSlope`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' random slope linear model for the longitudinal outcome.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkIdentity()`]
#' @exportClass LongitudinalRandomSlope
.LongitudinalRandomSlope <- setClass(
    Class = "LongitudinalRandomSlope",
    contains = "LongitudinalModel"
)


# LongitudinalRandomSlope-constructors ----

#' @rdname LongitudinalRandomSlope-class
#'
#' @typed intercept: Prior
#'   for the `intercept`.
#' @typed slope_mu: Prior
#'   for the population slope `slope_mu` (one per arm).
#' @typed slope_sigma: Prior
#'   for the random slope standard deviation `slope_sigma` (one per arm).
#' @typed sigma: Prior
#'   for the variance of the longitudinal values `sigma`.
#' @typed scaled_variance: logical
#'   whether the variance should be scaled by the expected value, corresponding to a multiplicative model.
#'   As a default, the variance is not scaled by the expected value, corresponding to an additive model.
#'   (See the "Statistical Specifications" vignette for more details.)
#'
#' @export
LongitudinalRandomSlope <- function(
    intercept = prior_normal(30, 10),
    slope_mu = prior_normal(1, 3),
    slope_sigma = prior_lognormal(0, 1.5),
    sigma = prior_lognormal(0, 1.5),
    scaled_variance = FALSE
) {
    stan <- StanModule(decorated_render(
        x = "lm-random-slope/model.stan",
        scaled_variance = scaled_variance
    ))

    # Apply constriants
    sigma <- set_limits(sigma, lower = 0)
    slope_sigma <- set_limits(slope_sigma, lower = 0)

    .LongitudinalRandomSlope(
        LongitudinalModel(
            name = "Random Slope",
            stan = stan,
            parameters = ParameterList(
                Parameter(
                    name = "lm_rs_intercept",
                    prior = intercept,
                    size = "n_studies"
                ),
                Parameter(
                    name = "lm_rs_slope_mu",
                    prior = slope_mu,
                    size = "n_arms"
                ),
                Parameter(
                    name = "lm_rs_slope_sigma",
                    prior = slope_sigma,
                    size = "n_arms"
                ),
                Parameter(name = "lm_rs_sigma", prior = sigma, size = 1),
                Parameter(
                    name = "lm_rs_ind_rnd_slope",
                    prior = prior_init_only(prior_normal(
                        median(slope_mu),
                        median(slope_sigma)
                    )),
                    size = "n_subjects"
                )
            )
        )
    )
}


#' @export
enableGQ.LongitudinalRandomSlope <- function(object, ...) {
    StanModule("lm-random-slope/quantities.stan")
}

#' @export
enableLink.LongitudinalRandomSlope <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-random-slope/link.stan")
    )
    object
}


#' @export
linkDSLD.LongitudinalRandomSlope <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-random-slope/link_dsld.stan"),
        prior = prior
    )
}

#' @export
linkIdentity.LongitudinalRandomSlope <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-random-slope/link_identity.stan"),
        prior = prior
    )
}

#' @export
linkGrowth.LongitudinalRandomSlope <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-random-slope/link_growth.stan"),
        prior = prior
    )
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.LongitudinalRandomSlope <- function(object, ...) {
    c("intercept", "slope")
}


#' @rdname getRandomEffectsNames
#' @export
getRandomEffectsNames.LongitudinalRandomSlope <- function(object, ...) {
    c("slope" = "lm_rs_ind_rnd_slope")
}
