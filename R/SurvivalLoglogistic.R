#' @include SurvivalModel.R
NULL

# SurvivalLogLogistic-class ----

#' `SurvivalLogLogistic`
#'
#' This class extends the general [`SurvivalModel`] class for using the
#' log-logistic survival model.
#'
#' @exportClass SurvivalLogLogistic
.SurvivalLogLogistic <- setClass(
    Class = "SurvivalLogLogistic",
    contains = "SurvivalModel"
)

# SurvivalLogLogistic-constructors ----

#' @rdname SurvivalLogLogistic-class
#'
#' @param a (`Prior`)\cr Prior distribution for the scale parameter `a`.
#' @param b (`Prior`)\cr Prior distribution for the shape parameter `b`.
#' @param beta (`Prior`)\cr Prior distribution for covariates coefficients `beta`.
#'
#' @export
SurvivalLogLogistic <- function(
    a = prior_lognormal(log(0.1), 5),
    b = prior_gamma(2, 5),
    beta = prior_normal(0, 2)
) {
    .SurvivalLogLogistic(
        SurvivalModel(
            name = "Log-Logistic",
            stan = StanModule("sm-loglogistic/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_loglogis_a", prior = a, size = 1),
                Parameter(name = "sm_loglogis_b", prior = b, size = 1),
                Parameter(name = "beta_os_cov", prior = beta, size = "p_os_cov_design")
            )
        )
    )
}
