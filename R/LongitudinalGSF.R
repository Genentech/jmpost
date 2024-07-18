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
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkTTG()`]
#' - [`linkIdentity()`]
#' - [`linkGrowth()`]
#' @exportClass LongitudinalGSF
.LongitudinalGSF <- setClass(
    Class = "LongitudinalGSF",
    contains = "LongitudinalModel"
)

# LongitudinalGSF-constructors ----

#' @rdname LongitudinalGSF-class
#'
#' @param mu_bsld (`Prior`)\cr for the mean baseline value `mu_bsld`.
#' @param mu_ks (`Prior`)\cr for the mean shrinkage rate `mu_ks`.
#' @param mu_kg (`Prior`)\cr for the mean growth rate `mu_kg`.
#' @param mu_phi (`Prior`)\cr for the mean proportion of cells affected by the treatment `mu_phi`.
#'
#' @param omega_bsld (`Prior`)\cr for the baseline value standard deviation `omega_bsld`.
#' @param omega_ks (`Prior`)\cr for the shrinkage rate standard deviation `omega_ks`.
#' @param omega_kg (`Prior`)\cr for the growth rate standard deviation `omega_kg`.
#' @param omega_phi (`Prior`)\cr for the standard deviation of the proportion of cells
#' affected by the treatment `omega_phi`.
#'
#' @param sigma (`Prior`)\cr for the variance of the longitudinal values `sigma`.
#'
#' @param centred (`logical`)\cr whether to use the centred parameterization.
#'
#' @importFrom stats qlogis
#' @export
LongitudinalGSF <- function(

    mu_bsld = prior_normal(log(60), 1),
    mu_ks = prior_normal(log(0.5), 1),
    mu_kg = prior_normal(log(0.3), 1),
    mu_phi = prior_normal(qlogis(0.5), 1),

    omega_bsld = prior_lognormal(log(0.2), 1),
    omega_ks = prior_lognormal(log(0.2), 1),
    omega_kg = prior_lognormal(log(0.2), 1),
    omega_phi = prior_lognormal(log(0.2), 1),

    sigma = prior_lognormal(log(0.1), 1),

    centred = FALSE
) {

    gsf_model <- StanModule(decorated_render(
        .x = read_stan("lm-gsf/model.stan"),
        centred = centred
    ))

    # Apply constraints
    omega_bsld <- set_limits(omega_bsld, lower = 0)
    omega_ks <- set_limits(omega_ks, lower = 0)
    omega_kg <- set_limits(omega_kg, lower = 0)
    omega_phi <- set_limits(omega_phi, lower = 0)
    sigma <- set_limits(sigma, lower = 0)


    parameters <- list(
        Parameter(name = "lm_gsf_mu_bsld", prior = mu_bsld, size = "n_studies"),
        Parameter(name = "lm_gsf_mu_ks", prior = mu_ks, size = "n_arms"),
        Parameter(name = "lm_gsf_mu_kg", prior = mu_kg, size = "n_arms"),
        Parameter(name = "lm_gsf_mu_phi", prior = mu_phi, size = "n_arms"),

        Parameter(name = "lm_gsf_omega_bsld", prior = omega_bsld, size = 1),
        Parameter(name = "lm_gsf_omega_ks", prior = omega_ks, size = 1),
        Parameter(name = "lm_gsf_omega_kg", prior = omega_kg, size = 1),
        Parameter(name = "lm_gsf_omega_phi", prior = omega_phi, size = 1),

        Parameter(name = "lm_gsf_sigma", prior = sigma, size = 1)
    )

    assert_flag(centred)
    parameters_extra <- if (centred) {
        list(
            Parameter(
                name = "lm_gsf_psi_bsld",
                prior = prior_init_only(prior_lognormal(median(mu_bsld), median(omega_bsld))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_gsf_psi_ks",
                prior = prior_init_only(prior_lognormal(median(mu_ks), median(omega_ks))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_gsf_psi_kg",
                prior = prior_init_only(prior_lognormal(median(mu_kg), median(omega_kg))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_gsf_psi_phi_logit",
                prior = prior_init_only(prior_normal(median(mu_phi), median(omega_phi))),
                size = "n_subjects"
            )
        )
    } else {
        list(
            Parameter(name = "lm_gsf_eta_tilde_bsld", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_gsf_eta_tilde_ks", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_gsf_eta_tilde_kg", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_gsf_eta_tilde_phi", prior = prior_std_normal(), size = "n_subjects")
        )
    }
    parameters <- append(parameters, parameters_extra)

    x <- LongitudinalModel(
        name = "Generalized Stein-Fojo",
        stan = merge(
            gsf_model,
            StanModule("lm-gsf/functions.stan")
        ),
        parameters = do.call(ParameterList, parameters)
    )
    .LongitudinalGSF(x)
}



#' @export
enableGQ.LongitudinalGSF <- function(object, ...) {
    StanModule("lm-gsf/quantities.stan")
}


#' @export
enableLink.LongitudinalGSF <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-gsf/link.stan")
    )
    object
}

#' @export
linkDSLD.LongitudinalGSF <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-gsf/link_dsld.stan"),
        prior = prior
    )
}

#' @export
linkTTG.LongitudinalGSF <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_ttg",
        stan = StanModule("lm-gsf/link_ttg.stan"),
        prior = prior
    )
}

#' @export
linkIdentity.LongitudinalGSF <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-gsf/link_identity.stan"),
        prior = prior
    )
}

#' @export
linkGrowth.LongitudinalGSF <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-gsf/link_growth.stan"),
        prior = prior
    )
}


#' @export
linkShrinkage.LongitudinalGSF <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_shrinkage",
        stan = StanModule("lm-gsf/link_shrinkage.stan"),
        prior = prior
    )
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.LongitudinalGSF <- function(object, ...) {
    c("b", "s", "g", "phi")
}
