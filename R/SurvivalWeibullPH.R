
#' @include SurvivalModel.R
NULL

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


#' @export
SurvivalWeibullPH <- function(
    lambda = prior_gamma(2, 0.5),
    gamma = prior_gamma(2, 0.5),
    beta = prior_normal(0, 5)
) {
    .SurvivalWeibullPH(
        SurvivalModel(
            stan = StanModule(x = "sm-weibull-ph/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_weibull_ph_lambda", prior = lambda, size = 1),
                Parameter(name = "sm_weibull_ph_gamma", prior = gamma, size = 1),
                Parameter(name = "beta_os_cov", prior = beta, size = "p_os_cov_design")
            )
        )
    )
}

