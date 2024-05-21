#' @include LongitudinalModel.R
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include Parameter.R
#' @include Link.R
NULL


#' `LongitudinalSteinFojo`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' Stein-Fojo model for the longitudinal outcome.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkTTG()`]
#' - [`linkIdentity()`]
#' @exportClass LongitudinalSteinFojo
.LongitudinalSteinFojo <- setClass(
    Class = "LongitudinalSteinFojo",
    contains = "LongitudinalModel"
)


#' @rdname LongitudinalSteinFojo-class
#'
#' @param mu_bsld (`Prior`)\cr for the mean baseline value `mu_bsld`.
#' @param mu_ks (`Prior`)\cr for the mean shrinkage rate `mu_ks`.
#' @param mu_kg (`Prior`)\cr for the mean growth rate `mu_kg`.
#'
#' @param omega_bsld (`Prior`)\cr for the baseline value standard deviation `omega_bsld`.
#' @param omega_ks (`Prior`)\cr for the shrinkage rate standard deviation `omega_ks`.
#' @param omega_kg (`Prior`)\cr for the growth rate standard deviation `omega_kg`.
#'
#' @param sigma (`Prior`)\cr for the variance of the longitudinal values `sigma`.
#'
#' @param centred (`logical`)\cr whether to use the centred parameterization.
#'
#' @export
LongitudinalSteinFojo <- function(

    mu_bsld = prior_normal(log(60), 1),
    mu_ks = prior_normal(log(0.5), 1),
    mu_kg = prior_normal(log(0.3), 1),

    omega_bsld = prior_lognormal(log(0.2), 1),
    omega_ks = prior_lognormal(log(0.2), 1),
    omega_kg = prior_lognormal(log(0.2), 1),

    sigma = prior_lognormal(log(0.1), 1),

    centred = FALSE
) {

    sf_model <- StanModule(decorated_render(
        .x = read_stan("lm-stein-fojo/model.stan"),
        centred = centred
    ))

    parameters <- list(
        Parameter(name = "lm_sf_mu_bsld", prior = mu_bsld, size = "n_studies"),
        Parameter(name = "lm_sf_mu_ks", prior = mu_ks, size = "n_arms"),
        Parameter(name = "lm_sf_mu_kg", prior = mu_kg, size = "n_arms"),

        Parameter(name = "lm_sf_omega_bsld", prior = omega_bsld, size = 1),
        Parameter(name = "lm_sf_omega_ks", prior = omega_ks, size = 1),
        Parameter(name = "lm_sf_omega_kg", prior = omega_kg, size = 1),

        Parameter(name = "lm_sf_sigma", prior = sigma, size = 1)
    )

    assert_flag(centred)
    parameters_extra <- if (centred) {
        list(
            Parameter(
                name = "lm_sf_psi_bsld",
                prior = prior_init_only(prior_lognormal(mu_bsld@init, omega_bsld@init)),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_sf_psi_ks",
                prior = prior_init_only(prior_lognormal(mu_ks@init, omega_ks@init)),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_sf_psi_kg",
                prior = prior_init_only(prior_lognormal(mu_kg@init, omega_kg@init)),
                size = "n_subjects"
            )
        )
    } else {
        list(
            Parameter(name = "lm_sf_eta_tilde_bsld", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_sf_eta_tilde_ks", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_sf_eta_tilde_kg", prior = prior_std_normal(), size = "n_subjects")
        )
    }
    parameters <- append(parameters, parameters_extra)

    x <- LongitudinalModel(
        name = "Stein-Fojo",
        stan = merge(
            sf_model,
            StanModule("lm-stein-fojo/functions.stan")
        ),
        parameters = do.call(ParameterList, parameters)
    )
    .LongitudinalSteinFojo(x)
}



#' @export
enableLink.LongitudinalSteinFojo <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-stein-fojo/link.stan")
    )
    object
}

#' @export
linkDSLD.LongitudinalSteinFojo <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-stein-fojo/link_dsld.stan"),
        prior = prior
    )
}

#' @export
linkTTG.LongitudinalSteinFojo <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_ttg",
        stan = StanModule("lm-stein-fojo/link_ttg.stan"),
        prior = prior
    )
}

#' @export
linkIdentity.LongitudinalSteinFojo <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-stein-fojo/link_identity.stan"),
        prior = prior
    )
}

#' @export
linkGrowth.LongitudinalSteinFojo <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-stein-fojo/link_growth.stan"),
        prior = prior
    )
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.LongitudinalSteinFojo <- function(object, ...) {
    c("b", "s", "g")
}
