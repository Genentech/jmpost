

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


#' @export
SurvivalWeibullPH <- function() {
    stan <- StanModule(
        x = "sm-weibull-ph/model.stan"
    )
    .SurvivalWeibullPH(
        SurvivalModel(stan = stan)
    )
}

