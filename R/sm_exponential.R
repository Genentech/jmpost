

.SurvivalExponential <- setClass(
    Class = "SurvivalExponential",
    contains = "SurvivalModel"
)

#' @export
SurvivalExponential <- function() {
    stan <- StanModule(
        x = "sm-exponential/model.stan"
    )
    .SurvivalExponential(
        SurvivalModel(stan = stan)
    )
}

