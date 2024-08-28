#' @include SurvivalModel.R
NULL


#' `SurvivalGamma`
#'
#' This class extends the general [`SurvivalModel`] class for using the
#' Gamma survival model.
#'
#' @exportClass SurvivalGamma
.SurvivalGamma <- setClass(
    Class = "SurvivalGamma",
    contains = "SurvivalModel"
)

# SurvivalGamma-constructors ----

#' @rdname SurvivalGamma-class
#'
#' @param k (`Prior`)\cr for the shape `k`.
#' @param theta (`Prior`)\cr for the scale `theta`.
#' @param beta (`Prior`)\cr for covariates coefficients `beta`.
#'
#' @export
SurvivalGamma <- function(
    k = prior_gamma(2, 0.5),
    theta = prior_gamma(2, 0.5),
    beta = prior_normal(0, 2)
) {

    k <- set_limits(k, lower = 0)
    theta <- set_limits(theta, lower = 0)

    .SurvivalGamma(
        SurvivalModel(
            name = "Gamma",
            stan = StanModule(x = "sm-gamma/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_gamma_k", prior = k, size = 1),
                Parameter(name = "sm_gamma_theta", prior = theta, size = 1),
                Parameter(name = "beta_os_cov", prior = beta, size = "p_os_cov_design")
            )
        )
    )
}
