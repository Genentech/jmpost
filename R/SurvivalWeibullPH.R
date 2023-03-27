
#' @include SurvivalModel.R
NULL

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


#' @export
SurvivalWeibullPH <- function(
    lambda = Parameter(prior_gamma(2, 0.5), 1/200),
    gamma = Parameter(prior_gamma(2, 0.5), 1)
) {
    .SurvivalWeibullPH(
        SurvivalModel(
            stan = StanModule(x = "sm-weibull-ph/model.stan"),
            parameters = ParameterList(
                sm_weibull_ph_lambda = lambda,
                sm_weibull_ph_gamma = gamma
            )
        )
    )
}

