

#' @include SurvivalModel.R
NULL

.SurvivalExponential <- setClass(
    Class = "SurvivalExponential",
    contains = "SurvivalModel"
)

#' @export
SurvivalExponential <- function(
    lambda = Parameter(prior_gamma(2, 5), init = 1 / 200)
) {
    .SurvivalExponential(
        SurvivalModel(
            stan = StanModule("sm-exponential/model.stan"),
            parameters = ParameterList(
                sm_exp_lambda = lambda
            )
        )
    )
}

