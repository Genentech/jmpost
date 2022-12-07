

.SurvivalWeibullPH <- setClass(
    Class = "SurvivalWeibullPH",
    contains = "SurvivalModel"
)


SurvivalWeibullPH <- function() {
    stan <- StanModule(
        x = "sm-weibull-ph/model.stan"
    )
    .SurvivalWeibullPH(
        SurvivalModel(stan = stan)
    )
}

