#' @include LongitudinalModel.R
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include Parameter.R
#' @include Link.R
NULL

# LongitudinalGSF-class ----

#' `LongitudinalGSF`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' Generalized Stein-Fojo (GSF) model for the longitudinal outcome.
#'
#' @exportClass LongitudinalGSF
.LongitudinalGSF <- setClass(
    Class = "LongitudinalGSF",
    contains = "LongitudinalModel"
)

# LongitudinalGSF-constructors ----

#' @rdname LongitudinalGSF-class
#'
#' @typed mu_bsld: Prior
#'   for the mean baseline value `mu_bsld`.
#' @typed mu_ks: Prior
#'   for the mean shrinkage rate `mu_ks`.
#' @typed mu_kg: Prior
#'   for the mean growth rate `mu_kg`.
#' @typed mu_phi: Prior
#'   for the mean shrinkage proportion `mu_phi`.
#' @typed omega_bsld: Prior
#'   for the baseline value standard deviation `omega_bsld`.
#' @typed omega_ks: Prior
#'   for the shrinkage rate standard deviation `omega_ks`.
#' @typed omega_kg: Prior
#'   for the growth rate standard deviation `omega_kg`.
#' @typed omega_phi: Prior
#'   for the shrinkage proportion standard deviation `omega_phi`.
#' @typed sigma: Prior
#'   for the variance of the longitudinal values `sigma`.
#'
#' @export
LongitudinalGSF <- function(
    mu_bsld = prior_lognormal(log(55), 5, init = 55),
    mu_ks = prior_lognormal(log(0.1), 0.5, init = 0.1),
    mu_kg = prior_lognormal(log(0.1), 1, init = 0.1),
    mu_phi = prior_beta(2, 8, init = 0.2),
    omega_bsld = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_ks = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_kg = prior_lognormal(log(0.1), 1, init = 0.1),
    omega_phi = prior_lognormal(log(0.1), 1, init = 0.1),
    sigma = prior_lognormal(log(0.1), 0.8, init = 0.1)
) {
    eta_prior <- prior_std_normal()
    x <- LongitudinalModel(
        stan = merge(
            StanModule("lm-gsf/model.stan"),
            StanModule("lm-gsf/functions.stan")
        ),
        parameters = ParameterList(
            Parameter(name = "lm_gsf_mu_bsld", prior = mu_bsld, size = "n_studies"),
            Parameter(name = "lm_gsf_mu_ks", prior = mu_ks, size = "n_arms"),
            Parameter(name = "lm_gsf_mu_kg", prior = mu_kg, size = "n_arms"),
            Parameter(name = "lm_gsf_mu_phi", prior = mu_phi, size = "n_arms"),
            Parameter(name = "lm_gsf_omega_bsld", prior = omega_bsld, size = 1),
            Parameter(name = "lm_gsf_omega_ks", prior = omega_ks, size = 1),
            Parameter(name = "lm_gsf_omega_kg", prior = omega_kg, size = 1),
            Parameter(name = "lm_gsf_omega_phi", prior = omega_phi, size = 1),
            Parameter(name = "lm_gsf_sigma", prior = sigma, size = 1),
            Parameter(name = "lm_gsf_eta_tilde_bsld", prior = eta_prior, size = "Nind"),
            Parameter(name = "lm_gsf_eta_tilde_ks", prior = eta_prior, size = "Nind"),
            Parameter(name = "lm_gsf_eta_tilde_kg", prior = eta_prior, size = "Nind"),
            Parameter(name = "lm_gsf_eta_tilde_phi", prior = eta_prior, size = "Nind")
        )
    )
    .LongitudinalGSF(x)
}
