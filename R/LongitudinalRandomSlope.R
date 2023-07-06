#' @include LongitudinalModel.R
#' @include Link.R
NULL

# LongitudinalRandomSlope-class ----

#' `LongitudinalRandomSlope`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' random slope linear model for the longitudinal outcome.
#'
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
#'   for the population slope `slope_mu`.
#' @typed slope_sigma: Prior
#'   for the random slope standard deviation `slope_sigma`.
#' @typed sigma: Prior
#'   for the variance of the longitudinal values `sigma`.
#' @typed random_slope: Prior
#'   must be `prior_none()`, just used to specify initial values.
#'
#' @export
LongitudinalRandomSlope <- function(
    intercept = prior_normal(30, 10, init = 30),
    slope_mu = prior_normal(0, 15, init = 0.001),
    slope_sigma = prior_lognormal(1, 5, init = 1),
    sigma = prior_lognormal(1, 5, init = 1),
    random_slope = prior_none(init = 0)
) {

    stan <- StanModule(
        x = "lm-random-slope/model.stan"
    )

    if (!as.character(random_slope) == "") {
        stop("`random_slope` must be a `prior_none()`")
    }

    .LongitudinalRandomSlope(
        LongitudinalModel(
            stan = stan,
            parameters = ParameterList(
                Parameter(name = "lm_rs_intercept", prior = intercept, size = 1),
                Parameter(name = "lm_rs_slope_mu", prior = slope_mu, size = "n_arms"),
                Parameter(name = "lm_rs_slope_sigma", prior = slope_sigma, size = 1),
                Parameter(name = "lm_rs_sigma", prior = sigma, size = 1),
                Parameter(name = "lm_rs_ind_rnd_slope", prior = random_slope, size = "Nind")
            )
        )
    )
}
