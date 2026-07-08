#' @include SurvivalModel.R
NULL

# SurvivalExponential-class ----

#' `SurvivalExponential`
#'
#' This class extends the general [`SurvivalModel`] class for using the
#' exponential survival model.
#'
#' @exportClass SurvivalExponential
.SurvivalExponential <- setClass(
    Class = "SurvivalExponential",
    contains = "SurvivalModel"
)

# SurvivalExponential-constructors ----

#' @rdname SurvivalExponential-class
#'
#' @typed lambda: Prior
#'   for the exponential rate `lambda`.
#' @typed beta: Prior
#'   for covariates coefficients `beta`.
#'
#' @export
SurvivalExponential <- function(
    lambda = prior_gamma(2, 5),
    beta = prior_normal(0, 2)
) {
    stan <- if (lambda@.is_const) {
        StanModule("sm-exponential/model_const_lambda.stan")
    } else {
        StanModule("sm-exponential/model.stan")
    }
    .SurvivalExponential(
        SurvivalModel(
            name = "Exponential",
            stan = stan,
            parameters = ParameterList(
                Parameter(name = "sm_exp_lambda", prior = lambda, size = 1),
                Parameter(
                    name = "beta_os_cov",
                    prior = beta,
                    size = "p_os_cov_design"
                )
            )
        )
    )
}
