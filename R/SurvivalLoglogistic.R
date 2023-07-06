#' @include SurvivalModel.R
NULL

# SurvivalLogLogistic-class ----

#' `SurvivalLogLogistic`
#'
#' This class extends the general [`SurvivalModel`] class for using the
#' log-logistic survival model.
#'
#' @exportClass SurvivalLogLogistic
.SurvivalLogLogistic <- setClass(
    Class = "SurvivalLogLogistic",
    contains = "SurvivalModel"
)

# SurvivalLogLogistic-constructors ----

#' @rdname SurvivalLogLogistic-class
#'
#' @typed lambda: Prior
#'   for the inverse median `lambda`.
#' @typed p: Prior
#'   for the shape parameter `p`.
#' @typed beta: Prior
#'   for covariates coefficients `beta`.
#'
#' @export
SurvivalLogLogistic <- function(
         lambda = prior_lognormal(log(0.1), 5, init = 0.1),
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
