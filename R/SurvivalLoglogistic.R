


#' @include SurvivalModel.R
NULL

.SurvivalLogLogistic <- setClass(
    Class = "SurvivalLogLogistic",
    contains = "SurvivalModel"
)

#' @export
SurvivalLogLogistic <- function(
         lambda = prior_lognormal(0, 5, init = 1 / 200),
         p = prior_gamma(2, 5, init = 0.5),
         beta = prior_normal(0, 5)
) {
    .SurvivalLogLogistic(
        SurvivalModel(
            stan = StanModule("sm-loglogistic/model.stan"),
            parameters = ParameterList(
                Parameter(name = "sm_logl_lambda", prior = lambda, size = 1),
                Parameter(name = "sm_logl_p", prior = p, size = 1),
                Parameter(name = "beta_os_cov", prior = beta, size = "p_os_cov_design")
            )
        )
    )
}




