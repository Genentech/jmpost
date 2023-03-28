
#' @include SurvivalModel.R
NULL

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


#' @export
SurvivalWeibullPH <- function(
    lambda = prior_gamma(2, 0.5, 1/200),
    gamma = prior_gamma(2, 0.5, 1),
    beta = prior_normal(0, 5)
) {
    .SurvivalWeibullPH(
        SurvivalModel(
            stan = StanModule(x = "sm-weibull-ph/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_weibull_ph_lambda", prior = lambda),
                Parameter(name = "sm_weibull_ph_gamma", prior = gamma),
                Parameter(name = "beta_os_cov", prior = beta)
            )
        )
    )
}

