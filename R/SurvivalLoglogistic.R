


#' @include SurvivalModel.R
NULL

.Survivalloglogistic <- setClass(
    Class = "Survivalloglogistic",
    contains = "SurvivalModel"
)

#' @export
Survivalloglogistic <- function(
         lamda = Parameter(prior_lognormal(0,5), init = 1 / 200), ## lognormal distributed prior
         p = Parameter(prior_gamma(2, 5), init = 0.5)
) {
    .Survivalloglogistic(
        SurvivalModel(
            stan = StanModule("sm-loglogistic/model.stan"),
            parameters = ParameterList(
                sm_loglogistic_ph_lamda = lamda,
                sm_loglogistic_ph_p =p
            )
        )
    )
}




