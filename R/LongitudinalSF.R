#' @include LongitudinalModel.R
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include Parameter.R
#' @include Link.R
NULL

# LongitudinalSF-class ----

#' `LongitudinalSF`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' Stein-Fojo (SF) model for the longitudinal outcome.
#'
#' @exportClass LongitudinalSF
.LongitudinalSF <- setClass(
    Class = "LongitudinalSF",
    contains = "LongitudinalModel"
)

# LongitudinalSF-constructors ----

#' @rdname LongitudinalSF-class
#'
#' @param mu_bsld (`Prior`)\cr for the mean baseline value `mu_bsld`.
#' @param mu_ks (`Prior`)\cr for the mean shrinkage rate `mu_ks`.
#' @param mu_kg (`Prior`)\cr for the mean growth rate `mu_kg`.
#' @param omega_bsld (`Prior`)\cr for the baseline value standard deviation `omega_bsld`.
#' @param omega_ks (`Prior`)\cr for the shrinkage rate standard deviation `omega_ks`.
#' @param omega_kg (`Prior`)\cr for the growth rate standard deviation `omega_kg`.
#' @param sigma (`Prior`)\cr for the variance of the longitudinal values `sigma`.
#'
#' @export
LongitudinalSF <- function(
    mu_bsld = prior_lognormal(log(55), 5, init = 55),
    mu_ks = prior_lognormal(log(0.1), 0.5, init = 0.1),
    mu_kg = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_bsld = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_ks = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_kg = prior_lognormal(log(0.1), 1, init = 0.1),
    sigma = prior_lognormal(log(0.1), 0.8, init = 0.1)
) {
    eta_prior <- prior_std_normal()
    x <- LongitudinalModel(
        stan = merge(
            StanModule("lm-sf/model.stan"),
            StanModule("lm-sf/functions.stan")
        ),
        parameters = ParameterList(
            Parameter(name = "lm_gsf_mu_bsld", prior = mu_bsld, size = "n_studies"),
            Parameter(name = "lm_gsf_mu_ks", prior = mu_ks, size = "n_arms"),
            Parameter(name = "lm_gsf_mu_kg", prior = mu_kg, size = "n_arms"),
            Parameter(name = "lm_gsf_omega_bsld", prior = omega_bsld, size = 1),
            Parameter(name = "lm_gsf_omega_ks", prior = omega_ks, size = 1),
            Parameter(name = "lm_gsf_omega_kg", prior = omega_kg, size = 1),
            Parameter(name = "lm_gsf_sigma", prior = sigma, size = 1),
            Parameter(name = "lm_gsf_eta_tilde_bsld", prior = eta_prior, size = "Nind"),
            Parameter(name = "lm_gsf_eta_tilde_ks", prior = eta_prior, size = "Nind"),
            Parameter(name = "lm_gsf_eta_tilde_kg", prior = eta_prior, size = "Nind")
        )
    )
    .LongitudinalSF(x)
}
