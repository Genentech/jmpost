

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


#' @export
SurvivalWeibullPH <- function(
    sm_weibull_ph_lambda = Parameter(gamma_prior(2, 0.5), 1/200),
    sm_weibull_ph_gamma = Parameter(gamma_prior(2, 0.5), 1)
) {
    .SurvivalWeibullPH(
        SurvivalModel(
            stan = StanModule(x = "sm-weibull-ph/model.stan"),
            pars = ParameterList(
                sm_weibull_ph_lambda = sm_weibull_ph_lambda,
                sm_weibull_ph_gamma = sm_weibull_ph_gamma
            )
        )
    )
}

