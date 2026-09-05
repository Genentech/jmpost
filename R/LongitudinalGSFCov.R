#' @include LongitudinalRandomSlopeCov.R
#' @include LongitudinalGSF.R
NULL

#' Generalized Stein-Fojo longitudinal model with subject-level covariates
#'
#' Separate covariate predictors model the log-normal distribution parameters
#' for baseline, shrinkage, growth, and the logit-normal treatment fraction.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkTTG()`]
#' - [`linkIdentity()`]
#' - [`linkGrowth()`]
#' - [`linkShrinkage()`]
#'
#' @exportClass LongitudinalGSFCov
.LongitudinalGSFCov <- setClass(
    "LongitudinalGSFCov",
    contains = "LongitudinalModel",
    slots = c(
        mu_b_formula = "formula",
        omega_b_formula = "formula",
        mu_s_formula = "formula",
        omega_s_formula = "formula",
        mu_g_formula = "formula",
        omega_g_formula = "formula",
        mu_phi_formula = "formula",
        omega_phi_formula = "formula",
        mu_b_parametrization = "character",
        omega_b_parametrization = "character",
        mu_s_parametrization = "character",
        omega_s_parametrization = "character",
        mu_g_parametrization = "character",
        omega_g_parametrization = "character",
        mu_phi_parametrization = "character",
        omega_phi_parametrization = "character",
        centred_baseline = "logical",
        centred_shrinkage = "logical",
        centred_growth = "logical",
        centred_phi = "logical"
    )
)

#' Construct a Generalized Stein-Fojo model with subject-level covariates
#'
#' Formula intercept columns are removed because each predictor has a separate
#' intercept. `linear`, `proportional`, `exponential`, and `log-linear`
#' parametrizations have the same meaning as in [LongitudinalSteinFojoCov()].
#'
#' @param mu_b_formula,omega_b_formula,mu_s_formula,omega_s_formula,mu_g_formula,omega_g_formula,mu_phi_formula,omega_phi_formula One-sided covariate formulas.
#' @param mu_b_parametrization,omega_b_parametrization,mu_s_parametrization,omega_s_parametrization,mu_g_parametrization,omega_g_parametrization,mu_phi_parametrization,omega_phi_parametrization Predictor parametrizations.
#' @param mu_b_intercept_prior,mu_b_coefficients_prior,omega_b_intercept_prior,omega_b_coefficients_prior,mu_s_intercept_prior,mu_s_coefficients_prior,omega_s_intercept_prior,omega_s_coefficients_prior,mu_g_intercept_prior,mu_g_coefficients_prior,omega_g_intercept_prior,omega_g_coefficients_prior,mu_phi_intercept_prior,mu_phi_coefficients_prior,omega_phi_intercept_prior,omega_phi_coefficients_prior Priors for predictor coefficients.
#' @param sigma Observation-error standard deviation prior.
#' @param scaled_variance Whether to use multiplicative observation error.
#' @param centred_baseline,centred_shrinkage,centred_growth,centred_phi Whether to use centred parameterizations.
#' @returns A `LongitudinalGSFCov` object.
#' @export
LongitudinalGSFCov <- function(
    mu_b_formula = ~study,
    omega_b_formula = ~study,
    mu_s_formula = ~arm,
    omega_s_formula = ~arm,
    mu_g_formula = ~arm,
    omega_g_formula = ~arm,
    mu_phi_formula = ~arm,
    omega_phi_formula = ~arm,
    mu_b_parametrization = "linear",
    omega_b_parametrization = "log-linear",
    mu_s_parametrization = "linear",
    omega_s_parametrization = "log-linear",
    mu_g_parametrization = "linear",
    omega_g_parametrization = "log-linear",
    mu_phi_parametrization = "logit-linear",
    omega_phi_parametrization = "log-linear",
    mu_b_intercept_prior = prior_normal(log(60), 1),
    mu_b_coefficients_prior = prior_normal(0, 1),
    omega_b_intercept_prior = prior_normal(log(0.2), 1),
    omega_b_coefficients_prior = prior_normal(0, 1),
    mu_s_intercept_prior = prior_normal(log(0.5), 1),
    mu_s_coefficients_prior = prior_normal(0, 1),
    omega_s_intercept_prior = prior_normal(log(0.2), 1),
    omega_s_coefficients_prior = prior_normal(0, 1),
    mu_g_intercept_prior = prior_normal(log(0.3), 1),
    mu_g_coefficients_prior = prior_normal(0, 1),
    omega_g_intercept_prior = prior_normal(log(0.2), 1),
    omega_g_coefficients_prior = prior_normal(0, 1),
    mu_phi_intercept_prior = prior_normal(qlogis(0.5), 1),
    mu_phi_coefficients_prior = prior_normal(0, 1),
    omega_phi_intercept_prior = prior_normal(log(0.2), 1),
    omega_phi_coefficients_prior = prior_normal(0, 1),
    sigma = prior_lognormal(log(0.1), 1),
    scaled_variance = FALSE,
    centred_baseline = TRUE,
    centred_shrinkage = FALSE,
    centred_growth = FALSE,
    centred_phi = FALSE
) {
    lapply(
        c(centred_baseline, centred_shrinkage, centred_growth, centred_phi),
        assert_flag
    )
    names <- c(
        "mu_b",
        "omega_b",
        "mu_s",
        "omega_s",
        "mu_g",
        "omega_g",
        "mu_phi",
        "omega_phi"
    )
    formulas <- Map(
        .validate_covariate_formula,
        mget(paste0(names, "_formula"), inherits = FALSE),
        paste0(names, "_formula")
    )
    names(formulas) <- names
    parametrizations <- Map(
        .validate_covariate_parametrization,
        mget(paste0(names, "_parametrization"), inherits = FALSE),
        paste0(names, "_parametrization")
    )
    names(parametrizations) <- names
    for (name in names) {
        prior_name <- paste0(name, "_intercept_prior")
        assign(
            prior_name,
            .positive_intercept_prior(
                get(prior_name, inherits = FALSE),
                parametrizations[[name]],
                positive = startsWith(name, "omega") &&
                    parametrizations[[name]] != "log-linear"
            )
        )
    }
    sigma <- set_limits(sigma, lower = getOption("jmpost.double_eps"))
    predictor <- function(name) {
        .covariate_predictor_stan(
            paste0("lm_gsfc_", name),
            parametrizations[[name]]
        )
    }
    stan <- StanModule(decorated_render(
        .x = read_stan("lm-gsf-cov/model.stan"),
        scaled_variance = scaled_variance,
        centred_baseline = centred_baseline,
        centred_shrinkage = centred_shrinkage,
        centred_growth = centred_growth,
        centred_phi = centred_phi,
        mu_b_predictor = predictor("mu_b"),
        omega_b_predictor = predictor("omega_b"),
        mu_s_predictor = predictor("mu_s"),
        omega_s_predictor = predictor("omega_s"),
        mu_g_predictor = predictor("mu_g"),
        omega_g_predictor = predictor("omega_g"),
        mu_phi_predictor = predictor("mu_phi"),
        omega_phi_predictor = predictor("omega_phi")
    ))
    intercept_priors <- mget(
        paste0(names, "_intercept_prior"),
        inherits = FALSE
    )
    names(intercept_priors) <- names
    coefficient_priors <- mget(
        paste0(names, "_coefficients_prior"),
        inherits = FALSE
    )
    names(coefficient_priors) <- names
    parameters <- unlist(
        lapply(names, function(name) {
            list(
                Parameter(
                    name = paste0("lm_gsfc_", name, "_intercept"),
                    prior = intercept_priors[[name]]
                ),
                Parameter(
                    name = paste0("lm_gsfc_", name, "_coefficients"),
                    prior = coefficient_priors[[name]],
                    size = paste0("p_lm_gsfc_", name)
                )
            )
        }),
        recursive = FALSE
    )
    subject_parameter <- function(name, centred) {
        if (!centred) {
            return(Parameter(
                name = paste0("lm_gsfc_eta_tilde_", name),
                prior = prior_std_normal(),
                size = "n_subjects"
            ))
        }
        if (identical(name, "phi")) {
            return(Parameter(
                name = "lm_gsfc_psi_phi_logit",
                prior = prior_init_only(prior_normal(
                    stats::qlogis(.predictor_reference_value(
                        intercept_priors$mu_phi,
                        parametrizations$mu_phi
                    )),
                    .predictor_reference_value(
                        intercept_priors$omega_phi,
                        parametrizations$omega_phi
                    )
                )),
                size = "n_subjects"
            ))
        }
        Parameter(
            name = paste0("lm_gsfc_psi_", name),
            prior = set_limits(
                prior_init_only(prior_lognormal(
                    .predictor_reference_value(
                        intercept_priors[[paste0("mu_", name)]],
                        parametrizations[[paste0("mu_", name)]]
                    ),
                    .predictor_reference_value(
                        intercept_priors[[paste0("omega_", name)]],
                        parametrizations[[paste0("omega_", name)]]
                    )
                )),
                lower = getOption("jmpost.double_eps")
            ),
            size = "n_subjects"
        )
    }
    parameters <- c(
        parameters,
        list(Parameter(name = "lm_gsfc_sigma", prior = sigma)),
        Map(
            subject_parameter,
            c("b", "s", "g", "phi"),
            c(centred_baseline, centred_shrinkage, centred_growth, centred_phi)
        )
    )
    .LongitudinalGSFCov(
        LongitudinalModel(
            name = "Generalized Stein-Fojo with Covariates",
            stan = merge(stan, StanModule("lm-gsf/functions.stan")),
            parameters = do.call(ParameterList, parameters),
            scaled_variance = scaled_variance
        ),
        mu_b_formula = formulas$mu_b,
        omega_b_formula = formulas$omega_b,
        mu_s_formula = formulas$mu_s,
        omega_s_formula = formulas$omega_s,
        mu_g_formula = formulas$mu_g,
        omega_g_formula = formulas$omega_g,
        mu_phi_formula = formulas$mu_phi,
        omega_phi_formula = formulas$omega_phi,
        mu_b_parametrization = parametrizations$mu_b,
        omega_b_parametrization = parametrizations$omega_b,
        mu_s_parametrization = parametrizations$mu_s,
        omega_s_parametrization = parametrizations$omega_s,
        mu_g_parametrization = parametrizations$mu_g,
        omega_g_parametrization = parametrizations$omega_g,
        mu_phi_parametrization = parametrizations$mu_phi,
        omega_phi_parametrization = parametrizations$omega_phi,
        centred_baseline = centred_baseline,
        centred_shrinkage = centred_shrinkage,
        centred_growth = centred_growth,
        centred_phi = centred_phi
    )
}

#' @export
enableGQ.LongitudinalGSFCov <- function(
    object,
    generator = NULL,
    type = NULL,
    ...
) {
    idv <- identical(type, "longitudinal") &&
        is(generator, "QuantityGeneratorSubject")
    pop <- identical(type, "longitudinal") &&
        is(generator, "QuantityGeneratorPopulation")
    predictor <- function(name) {
        .covariate_predictor_stan(
            paste0("lm_gsfc_", name),
            slot(object, paste0(name, "_parametrization")),
            design_prefix = paste0("gq_lm_gsfc_", name),
            n_rows = "gq_n_quant"
        )
    }
    StanModule(decorated_render(
        .x = read_stan("lm-gsf-cov/quantities.stan"),
        include_gq_longitudinal_idv = idv,
        include_gq_longitudinal_pop = pop,
        mu_b_population_predictor = predictor("mu_b"),
        mu_s_population_predictor = predictor("mu_s"),
        mu_g_population_predictor = predictor("mu_g"),
        mu_phi_population_predictor = predictor("mu_phi")
    ))
}

#' @export
gq_population_stan_data.LongitudinalGSFCov <- function(
    object,
    model,
    data = NULL,
    ...
) {
    names <- c("mu_b", "mu_s", "mu_g", "mu_phi")
    result <- list(
        declarations = paste(
            sprintf(
                "matrix[gq_n_quant, p_lm_gsfc_%s] gq_lm_gsfc_%s_design;",
                names,
                names
            ),
            collapse = "\n"
        ),
        data = list()
    )
    if (!is.null(data)) {
        assert_that(
            !is.null(object@newdata) &&
                nrow(object@newdata) == length(object@times),
            msg = "Population quantities for `LongitudinalGSFCov` require `GridPopulation(newdata = ...)`"
        )
        subject_data <- as.data.frame(harmonise(data@subject))
        result$data <- setNames(
            lapply(names, function(name) {
                .covariate_prediction_design_matrix(
                    slot(model, paste0(name, "_formula")),
                    object@newdata,
                    subject_data,
                    paste0(name, "_formula")
                )
            }),
            paste0("gq_lm_gsfc_", names, "_design")
        )
    }
    result
}

#' @export
enableLink.LongitudinalGSFCov <- function(object, ...) {
    object@stan <- merge(object@stan, StanModule("lm-gsf-cov/link.stan"))
    object
}

#' @export
linkDSLD.LongitudinalGSFCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-gsf/link_dsld.stan"),
        prior = prior
    )
}
#' @export
linkTTG.LongitudinalGSFCov <- function(prior = prior_normal(0, 2), model, ...) {
    LinkComponent(
        key = "link_ttg",
        stan = StanModule("lm-gsf/link_ttg.stan"),
        prior = prior
    )
}
#' @export
linkIdentity.LongitudinalGSFCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-gsf/link_identity.stan"),
        prior = prior
    )
}
#' @export
linkGrowth.LongitudinalGSFCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-gsf/link_growth.stan"),
        prior = prior
    )
}
#' @export
linkShrinkage.LongitudinalGSFCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_shrinkage",
        stan = StanModule("lm-gsf/link_shrinkage.stan"),
        prior = prior
    )
}
#' @export
getPredictionNames.LongitudinalGSFCov <- function(object, ...) {
    c("b", "s", "g", "phi")
}
#' @export
getRandomEffectsNames.LongitudinalGSFCov <- function(object, ...) {
    c(
        b = "lm_gsfc_psi_b",
        s = "lm_gsfc_psi_s",
        g = "lm_gsfc_psi_g",
        phi = "lm_gsfc_psi_phi"
    )
}
#' @export
longitudinal_model_stan_data.LongitudinalGSFCov <- function(model, subject) {
    subject_data <- as.data.frame(harmonise(subject))
    names <- c(
        "mu_b",
        "omega_b",
        "mu_s",
        "omega_s",
        "mu_g",
        "omega_g",
        "mu_phi",
        "omega_phi"
    )
    designs <- setNames(
        lapply(names, function(name) {
            .covariate_design_matrix(
                slot(model, paste0(name, "_formula")),
                subject_data,
                paste0(name, "_formula")
            )
        }),
        names
    )
    unlist(
        lapply(names, function(name) {
            setNames(
                list(ncol(designs[[name]]), designs[[name]]),
                c(
                    paste0("p_lm_gsfc_", name),
                    paste0("lm_gsfc_", name, "_design")
                )
            )
        }),
        recursive = FALSE
    )
}
#' @export
required_longitudinal_covs.LongitudinalGSFCov <- function(object, ...) {
    unique(unlist(lapply(c("mu_b", "mu_s", "mu_g", "mu_phi"), function(name) {
        all.vars(slot(object, paste0(name, "_formula")))
    })))
}
#' @export
required_simulation_covariates.LongitudinalGSFCov <- function(object, ...) {
    unique(unlist(lapply(
        c(
            "mu_b",
            "omega_b",
            "mu_s",
            "omega_s",
            "mu_g",
            "omega_g",
            "mu_phi",
            "omega_phi"
        ),
        function(name) all.vars(slot(object, paste0(name, "_formula")))
    )))
}
