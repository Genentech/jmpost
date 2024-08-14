#' @include SurvivalModel.R
NULL

# SurvivalWeibullPH-class ----

#' `SurvivalWeibullPH`
#'
#' This class extends the general [`SurvivalModel`] class for using the
#' Weibull proportional hazards survival model.
#'
#' @exportClass SurvivalWeibullPH
.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)

# SurvivalWeibullPH-constructors ----

#' @rdname SurvivalWeibullPH-class
#'
#' @param lambda (`Prior`)\cr for the scale `lambda`.
#' @param gamma (`Prior`)\cr for the shape `gamma`.
#' @param beta (`Prior`)\cr for covariates coefficients `beta`.
#'
#' @export
SurvivalWeibullPH <- function(
    lambda = prior_gamma(2, 0.5),
    gamma = prior_gamma(2, 0.5),
    beta = prior_normal(0, 2)
) {

    lambda <- set_limits(lambda, lower = 0)
    gamma <- set_limits(gamma, lower = 0)

    .SurvivalWeibullPH(
        SurvivalModel(
            name = "Weibull-PH",
            stan = StanModule(x = "sm-weibull-ph/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_weibull_ph_lambda", prior = lambda, size = 1),
                Parameter(name = "sm_weibull_ph_gamma", prior = gamma, size = 1),
                Parameter(name = "beta_os_cov", prior = beta, size = "p_os_cov_design")
            )
        )
    )
}
