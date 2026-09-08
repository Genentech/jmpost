#' @include LongitudinalRandomSlopeCov.R
#' @include LongitudinalClaretBruno.R
NULL

#' Claret-Bruno longitudinal model with subject-level covariates
#'
#' Separate covariate predictors model the log-normal distribution parameters
#' for baseline tumour size, growth, resistance, and growth inhibition.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkTTG()`]
#' - [`linkIdentity()`]
#' - [`linkGrowth()`]
#' @exportClass LongitudinalClaretBrunoCov
.LongitudinalClaretBrunoCov <- setClass(
    "LongitudinalClaretBrunoCov",
    contains = "LongitudinalModel",
    slots = c(
        mu_b_formula = "formula",
        omega_b_formula = "formula",
        mu_g_formula = "formula",
        omega_g_formula = "formula",
        mu_c_formula = "formula",
        omega_c_formula = "formula",
        mu_p_formula = "formula",
        omega_p_formula = "formula",
        mu_b_parametrization = "character",
        omega_b_parametrization = "character",
        mu_g_parametrization = "character",
        omega_g_parametrization = "character",
        mu_c_parametrization = "character",
        omega_c_parametrization = "character",
        mu_p_parametrization = "character",
        omega_p_parametrization = "character",
        centred_baseline = "logical",
        centred_growth = "logical",
        centred_resistance = "logical",
        centred_inhibition = "logical"
    )
)

#' Construct a Claret-Bruno model with subject-level covariates
#'
#' Formula intercept columns are removed because every predictor has a separate
#' intercept. Predictor parametrizations have the same meaning as in
#' [LongitudinalSteinFojoCov()].
#'
#' @param mu_b_formula,mu_g_formula,mu_c_formula,mu_p_formula
#'   One-sided covariate formulas for the mean parameters.
#' @param omega_b_formula,omega_g_formula,omega_c_formula,omega_p_formula
#'   One-sided covariate formulas for the variance parameters.
#' @param mu_b_parametrization,mu_g_parametrization,mu_c_parametrization,mu_p_parametrization
#'   Parametrization for the mean parameters.
#' @param omega_b_parametrization,omega_g_parametrization,omega_c_parametrization,omega_p_parametrization
#'   Parametrization for the variance parameters.
#' @param mu_b_intercept_prior,mu_g_intercept_prior,mu_c_intercept_prior,mu_p_intercept_prior
#'   Priors for the intercepts of the mean parameters.
#' @param mu_b_coefficients_prior,mu_g_coefficients_prior,mu_c_coefficients_prior,mu_p_coefficients_prior
#'   Priors for the coefficients of the mean parameters.
#' @param omega_b_intercept_prior,omega_g_intercept_prior,omega_c_intercept_prior,omega_p_intercept_prior
#'   Priors for the intercepts of the variance parameters.
#' @param omega_b_coefficients_prior,omega_g_coefficients_prior,omega_c_coefficients_prior,omega_p_coefficients_prior
#'   Priors for the coefficients of the variance parameters.
#' @param sigma Observation-error standard deviation prior.
#' @param scaled_variance Whether to use multiplicative observation error.
#' @param centred_baseline,centred_growth,centred_resistance,centred_inhibition Whether to use centred
#'   parameterizations.
#' @returns A `LongitudinalClaretBrunoCov` object.
#' @export
LongitudinalClaretBrunoCov <- function(
    mu_b_formula = ~study,
    omega_b_formula = ~study,
    mu_g_formula = ~arm,
    omega_g_formula = ~arm,
    mu_c_formula = ~arm,
    omega_c_formula = ~arm,
    mu_p_formula = ~arm,
    omega_p_formula = ~arm,
    mu_b_parametrization = "linear",
    omega_b_parametrization = "log-linear",
    mu_g_parametrization = "linear",
    omega_g_parametrization = "log-linear",
    mu_c_parametrization = "linear",
    omega_c_parametrization = "log-linear",
    mu_p_parametrization = "linear",
    omega_p_parametrization = "log-linear",
    mu_b_intercept_prior = prior_normal(log(60), 1),
    mu_b_coefficients_prior = prior_normal(0, 1),
    omega_b_intercept_prior = prior_normal(log(0.2), 1),
    omega_b_coefficients_prior = prior_normal(0, 1),
    mu_g_intercept_prior = prior_normal(log(1), 1),
    mu_g_coefficients_prior = prior_normal(0, 1),
    omega_g_intercept_prior = prior_normal(log(0.2), 1),
    omega_g_coefficients_prior = prior_normal(0, 1),
    mu_c_intercept_prior = prior_normal(log(0.4), 1),
    mu_c_coefficients_prior = prior_normal(0, 1),
    omega_c_intercept_prior = prior_normal(log(0.2), 1),
    omega_c_coefficients_prior = prior_normal(0, 1),
    mu_p_intercept_prior = prior_normal(log(2), 1),
    mu_p_coefficients_prior = prior_normal(0, 1),
    omega_p_intercept_prior = prior_normal(log(0.2), 1),
    omega_p_coefficients_prior = prior_normal(0, 1),
    sigma = prior_lognormal(log(0.1), 1),
    scaled_variance = FALSE,
    centred_baseline = TRUE,
    centred_growth = FALSE,
    centred_resistance = FALSE,
    centred_inhibition = FALSE
) {
    lapply(
        c(
            centred_baseline,
            centred_growth,
            centred_resistance,
            centred_inhibition
        ),
        assert_flag
    )
    names <- c(
        "mu_b",
        "omega_b",
        "mu_g",
        "omega_g",
        "mu_c",
        "omega_c",
        "mu_p",
        "omega_p"
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
            paste0("lm_clbrc_", name),
            parametrizations[[name]]
        )
    }
    predictor_args <- stats::setNames(
        lapply(names, predictor),
        paste0(names, "_predictor")
    )
    stan <- StanModule(do.call(
        decorated_render,
        c(
            list(
                .x = read_stan("lm-claret-bruno-cov/model.stan"),
                scaled_variance = scaled_variance,
                centred_baseline = centred_baseline,
                centred_growth = centred_growth,
                centred_resistance = centred_resistance,
                centred_inhibition = centred_inhibition
            ),
            predictor_args
        )
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
                    name = paste0("lm_clbrc_", name, "_intercept"),
                    prior = intercept_priors[[name]]
                ),
                Parameter(
                    name = paste0("lm_clbrc_", name, "_coefficients"),
                    prior = coefficient_priors[[name]],
                    size = paste0("p_lm_clbrc_", name)
                )
            )
        }),
        recursive = FALSE
    )
    subject_parameter <- function(name, centred) {
        if (!centred) {
            return(Parameter(
                name = paste0("lm_clbrc_eta_tilde_", name),
                prior = prior_std_normal(),
                size = "n_subjects"
            ))
        }
        Parameter(
            name = paste0("lm_clbrc_psi_", name),
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
        list(Parameter(name = "lm_clbrc_sigma", prior = sigma)),
        unname(Map(
            subject_parameter,
            c("b", "g", "c", "p"),
            c(
                centred_baseline,
                centred_growth,
                centred_resistance,
                centred_inhibition
            )
        ))
    )
    args <- c(
        list(LongitudinalModel(
            name = "Claret-Bruno with Covariates",
            stan = merge(stan, StanModule("lm-claret-bruno/functions.stan")),
            parameters = do.call(ParameterList, parameters),
            scaled_variance = scaled_variance
        )),
        stats::setNames(formulas, paste0(names, "_formula")),
        stats::setNames(parametrizations, paste0(names, "_parametrization")),
        list(
            centred_baseline = centred_baseline,
            centred_growth = centred_growth,
            centred_resistance = centred_resistance,
            centred_inhibition = centred_inhibition
        )
    )
    do.call(.LongitudinalClaretBrunoCov, args)
}

#' @export
enableGQ.LongitudinalClaretBrunoCov <- function(
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
            paste0("lm_clbrc_", name),
            slot(object, paste0(name, "_parametrization")),
            design_prefix = paste0("gq_lm_clbrc_", name),
            n_rows = "gq_n_quant"
        )
    }
    StanModule(decorated_render(
        .x = read_stan("lm-claret-bruno-cov/quantities.stan"),
        include_gq_longitudinal_idv = idv,
        include_gq_longitudinal_pop = pop,
        mu_b_population_predictor = predictor("mu_b"),
        mu_g_population_predictor = predictor("mu_g"),
        mu_c_population_predictor = predictor("mu_c"),
        mu_p_population_predictor = predictor("mu_p")
    ))
}

#' @export
gq_population_stan_data.LongitudinalClaretBrunoCov <- function(
    object,
    model,
    data = NULL,
    ...
) {
    names <- c("mu_b", "mu_g", "mu_c", "mu_p")
    result <- list(
        declarations = paste(
            sprintf(
                "matrix[gq_n_quant, p_lm_clbrc_%s] gq_lm_clbrc_%s_design;",
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
            msg = "Population quantities for `LongitudinalClaretBrunoCov` require `GridPopulation(newdata = ...)`"
        )
        subject_data <- as.data.frame(harmonise(data@subject))
        result$data <- stats::setNames(
            lapply(names, function(name) {
                .covariate_prediction_design_matrix(
                    slot(model, paste0(name, "_formula")),
                    object@newdata,
                    subject_data,
                    paste0(name, "_formula")
                )
            }),
            paste0("gq_lm_clbrc_", names, "_design")
        )
    }
    result
}

#' @export
enableLink.LongitudinalClaretBrunoCov <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-claret-bruno-cov/link.stan")
    )
    object
}
#' @export
linkDSLD.LongitudinalClaretBrunoCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-claret-bruno/link_dsld.stan"),
        prior = prior
    )
}
#' @export
linkTTG.LongitudinalClaretBrunoCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_ttg",
        stan = StanModule("lm-claret-bruno/link_ttg.stan"),
        prior = prior
    )
}
#' @export
linkIdentity.LongitudinalClaretBrunoCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-claret-bruno/link_identity.stan"),
        prior = prior
    )
}
#' @export
linkGrowth.LongitudinalClaretBrunoCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-claret-bruno/link_growth.stan"),
        prior = prior
    )
}
#' @export
getPredictionNames.LongitudinalClaretBrunoCov <- function(object, ...) {
    c("b", "g", "c", "p")
}
#' @export
getRandomEffectsNames.LongitudinalClaretBrunoCov <- function(object, ...) {
    c(
        b = "lm_clbrc_psi_b",
        g = "lm_clbrc_psi_g",
        c = "lm_clbrc_psi_c",
        p = "lm_clbrc_psi_p"
    )
}

#' @export
longitudinal_model_stan_data.LongitudinalClaretBrunoCov <- function(
    model,
    subject
) {
    subject_data <- as.data.frame(harmonise(subject))
    names <- c(
        "mu_b",
        "omega_b",
        "mu_g",
        "omega_g",
        "mu_c",
        "omega_c",
        "mu_p",
        "omega_p"
    )
    designs <- stats::setNames(
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
            stats::setNames(
                list(ncol(designs[[name]]), designs[[name]]),
                c(
                    paste0("p_lm_clbrc_", name),
                    paste0("lm_clbrc_", name, "_design")
                )
            )
        }),
        recursive = FALSE
    )
}
#' @export
required_longitudinal_covs.LongitudinalClaretBrunoCov <- function(object, ...) {
    unique(unlist(lapply(c("mu_b", "mu_g", "mu_c", "mu_p"), function(name) {
        all.vars(slot(object, paste0(name, "_formula")))
    })))
}
#' @export
required_simulation_covariates.LongitudinalClaretBrunoCov <- function(
    object,
    ...
) {
    unique(unlist(lapply(
        c(
            "mu_b",
            "omega_b",
            "mu_g",
            "omega_g",
            "mu_c",
            "omega_c",
            "mu_p",
            "omega_p"
        ),
        function(name) all.vars(slot(object, paste0(name, "_formula")))
    )))
}
