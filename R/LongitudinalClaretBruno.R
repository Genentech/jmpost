#' @include LongitudinalModel.R
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include Parameter.R
#' @include Link.R
NULL


#' `LongitudinalClaretBruno`
#'
#' This class extends the general [`LongitudinalModel`] class for using the
#' Claret-Bruno model for the longitudinal outcome.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkTTG()`]
#' - [`linkIdentity()`]
#' - [`linkGrowth()`]
#' @exportClass LongitudinalClaretBruno
.LongitudinalClaretBruno <- setClass(
    Class = "LongitudinalClaretBruno",
    contains = "LongitudinalModel"
)


#' @rdname LongitudinalClaretBruno-class
#'
#' @param mu_b (`Prior`)\cr for the mean population baseline sld value.
#' @param mu_g (`Prior`)\cr for the mean population growth rate.
#' @param mu_c (`Prior`)\cr for the mean population resistance rate.
#' @param mu_p (`Prior`)\cr for the mean population growth inhibition
#'
#' @param omega_b (`Prior`)\cr for the population standard deviation for the baseline sld value.
#' @param omega_g (`Prior`)\cr for the population standard deviation for the growth rate.
#' @param omega_c (`Prior`)\cr for the population standard deviation for the resistance rate.
#' @param omega_p (`Prior`)\cr for the population standard deviation for the growth inhibition.
#'
#' @param sigma (`Prior`)\cr for the variance of the longitudinal values.
#'
#' @param centred (`logical`)\cr whether to use the centred parameterization.
#'
#' @export
LongitudinalClaretBruno <- function(

    mu_b = prior_normal(log(60), 0.5),
    mu_g = prior_normal(log(1), 0.5),
    mu_c = prior_normal(log(0.4), 0.5),
    mu_p = prior_normal(log(2), 0.5),

    omega_b = prior_lognormal(log(0.2), 0.5),
    omega_g = prior_lognormal(log(0.2), 0.5),
    omega_c = prior_lognormal(log(0.2), 0.5),
    omega_p = prior_lognormal(log(0.2), 0.5),

    sigma = prior_lognormal(log(0.1), 0.5),

    centred = FALSE
) {

    sf_model <- StanModule(decorated_render(
        .x = read_stan("lm-claret-bruno/model.stan"),
        centred = centred
    ))

    # Apply constraints
    omega_b <- set_limits(omega_b, lower = 0)
    omega_g <- set_limits(omega_g, lower = 0)
    omega_c <- set_limits(omega_c, lower = 0)
    omega_p <- set_limits(omega_p, lower = 0)
    sigma <- set_limits(sigma, lower = 0)


    parameters <- list(
        Parameter(name = "lm_clbr_mu_b", prior = mu_b, size = "n_studies"),
        Parameter(name = "lm_clbr_mu_g", prior = mu_g, size = "n_arms"),
        Parameter(name = "lm_clbr_mu_c", prior = mu_c, size = "n_arms"),
        Parameter(name = "lm_clbr_mu_p", prior = mu_p, size = "n_arms"),

        Parameter(name = "lm_clbr_omega_b", prior = omega_b, size = 1),
        Parameter(name = "lm_clbr_omega_g", prior = omega_g, size = 1),
        Parameter(name = "lm_clbr_omega_c", prior = omega_c, size = 1),
        Parameter(name = "lm_clbr_omega_p", prior = omega_p, size = 1),

        Parameter(name = "lm_clbr_sigma", prior = sigma, size = 1)
    )

    assert_flag(centred)
    parameters_extra <- if (centred) {
        list(
            Parameter(
                name = "lm_clbr_ind_b",
                prior = prior_init_only(prior_lognormal(median(mu_b), median(omega_b))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_clbr_ind_g",
                prior = prior_init_only(prior_lognormal(median(mu_g), median(omega_g))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_clbr_ind_c",
                prior = prior_init_only(prior_lognormal(median(mu_c), median(omega_c))),
                size = "n_subjects"
            ),
            Parameter(
                name = "lm_clbr_ind_p",
                prior = prior_init_only(prior_lognormal(median(mu_p), median(omega_p))),
                size = "n_subjects"
            )
        )
    } else {
        list(
            Parameter(name = "lm_clbr_eta_b", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_clbr_eta_g", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_clbr_eta_c", prior = prior_std_normal(), size = "n_subjects"),
            Parameter(name = "lm_clbr_eta_p", prior = prior_std_normal(), size = "n_subjects")
        )
    }
    parameters <- append(parameters, parameters_extra)

    x <- LongitudinalModel(
        name = "Claret-Bruno",
        stan = merge(
            sf_model,
            StanModule("lm-claret-bruno/functions.stan")
        ),
        parameters = do.call(ParameterList, parameters)
    )
    .LongitudinalClaretBruno(x)
}



#' @export
enableLink.LongitudinalClaretBruno <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-claret-bruno/link.stan")
    )
    object
}

#' @export
linkDSLD.LongitudinalClaretBruno <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-claret-bruno/link_dsld.stan"),
        prior = prior
    )
}

#' @export
linkTTG.LongitudinalClaretBruno <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_ttg",
        stan = StanModule("lm-claret-bruno/link_ttg.stan"),
        prior = prior
    )
}

#' @export
linkIdentity.LongitudinalClaretBruno <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-claret-bruno/link_identity.stan"),
        prior = prior
    )
}

#' @export
linkGrowth.LongitudinalClaretBruno <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-claret-bruno/link_growth.stan"),
        prior = prior
    )
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.LongitudinalClaretBruno <- function(object, ...) {
    c("b", "g", "c", "p")
}
