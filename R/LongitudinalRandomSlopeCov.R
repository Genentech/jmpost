#' @include LongitudinalRandomSlope.R
NULL

#' Random-slope longitudinal model with subject-level covariates
#'
#' Extends the random-slope longitudinal model by modelling the subject
#' intercept, mean slope, and random-slope standard deviation using separate
#' subject-level design matrices.
#'
#' @section Available Links:
#' - [`linkDSLD()`]
#' - [`linkIdentity()`]
#' - [`linkGrowth()`]
#'
#' @slot mu_formula (`formula`) Formula for the subject intercept.
#' @slot slope_mu_formula (`formula`) Formula for the mean slope.
#' @slot slope_sigma_formula (`formula`) Formula for the random-slope standard deviation.
#' @slot mu_parametrization (`character`) Parametrization for the subject intercept.
#' @slot slope_mu_parametrization (`character`) Parametrization for the mean slope.
#' @slot slope_sigma_parametrization (`character`) Parametrization for the slope standard deviation.
#'
#' @exportClass LongitudinalRandomSlopeCov
.LongitudinalRandomSlopeCov <- setClass(
    Class = "LongitudinalRandomSlopeCov",
    contains = "LongitudinalModel",
    slots = c(
        mu_formula = "formula",
        slope_mu_formula = "formula",
        slope_sigma_formula = "formula",
        mu_parametrization = "character",
        slope_mu_parametrization = "character",
        slope_sigma_parametrization = "character"
    )
)

#' Supported covariate-predictor parametrizations
#'
#' Names of the parametrizations accepted by the covariate-based longitudinal
#' models and simulators.
#'
#' @keywords internal
#' @returns A character vector.
.longitudinal_cov_parametrizations <- c(
    "linear",
    "proportional",
    "exponential",
    "log-linear"
)

#' Validate a longitudinal covariate formula
#'
#' @param x The object to validate as a one-sided formula.
#' @param argument Name of the user-facing argument, used in error messages.
#'
#' @keywords internal
#' @returns `x`, validated as a one-sided formula.
.validate_covariate_formula <- function(x, argument) {
    assert_formula(x)
    assert_that(
        length(x) == 2,
        msg = sprintf("`%s` must be a one-sided formula", argument)
    )
    x
}

#' Validate a covariate-predictor parametrization
#'
#' @param x Character scalar naming the parametrization.
#' @param argument Name of the user-facing argument, used in error messages.
#'
#' @keywords internal
#' @returns `x`, validated against the supported parametrizations.
.validate_covariate_parametrization <- function(x, argument) {
    assert_string(x, na.ok = FALSE)
    assert_that(
        x %in% .longitudinal_cov_parametrizations,
        msg = sprintf(
            "`%s` must be one of %s",
            argument,
            paste(
                sprintf("`%s`", .longitudinal_cov_parametrizations),
                collapse = ", "
            )
        )
    )
    x
}

#' Build a subject-level covariate design matrix
#'
#' Builds a model matrix and removes its intercept column. Single-level factors
#' are supported and contribute no columns under reference coding.
#'
#' @param formula One-sided formula describing the subject-level covariates.
#' @param data Data frame containing one row per subject.
#' @param argument Name of the user-facing formula argument, used in errors.
#'
#' @keywords internal
#' @returns A numeric design matrix without an intercept column.
.covariate_design_matrix <- function(formula, data, argument = "formula") {
    variables <- all.vars(formula)
    missing_variables <- setdiff(variables, names(data))
    assert_that(
        length(missing_variables) == 0,
        msg = sprintf(
            "All variables in `%s` must be present in the subject data; missing: %s",
            argument,
            paste(missing_variables, collapse = ", ")
        )
    )

    model_frame <- stats::model.frame(
        formula,
        data = data,
        na.action = stats::na.fail
    )
    sentinel_level <- ".__jmpost_unused_level__"
    for (variable in names(model_frame)) {
        values <- model_frame[[variable]]
        if (is.character(values)) {
            values <- factor(values)
        }
        if (is.factor(values) && nlevels(values) == 1) {
            levels(values) <- c(levels(values), sentinel_level)
        }
        model_frame[[variable]] <- values
    }
    design <- stats::model.matrix(formula, data = model_frame)
    remove_columns <- colnames(design) == "(Intercept)" |
        grepl(sentinel_level, colnames(design), fixed = TRUE)
    if (any(remove_columns)) {
        design <- design[, !remove_columns, drop = FALSE]
    }
    rownames(design) <- NULL
    design
}

#' Render a covariate predictor as Stan code
#'
#' @param prefix Prefix shared by the intercept and coefficient Stan variables.
#' @param parametrization Character scalar naming the predictor
#'   parametrization.
#' @param design_prefix Prefix for the design-matrix Stan variable.
#' @param n_rows Stan expression giving the number of predictor rows.
#'
#' @keywords internal
#' @returns A character scalar containing a Stan expression.
.covariate_predictor_stan <- function(
    prefix,
    parametrization,
    design_prefix = prefix,
    n_rows = "n_subjects"
) {
    intercept <- paste0(prefix, "_intercept")
    design <- paste0(design_prefix, "_design")
    coefficients <- paste0(prefix, "_coefficients")

    switch(
        parametrization,
        linear = sprintf(
            "rep_vector(%s, %s) + %s * %s",
            intercept,
            n_rows,
            design,
            coefficients
        ),
        proportional = sprintf(
            "%s * (rep_vector(1, %s) + %s * %s)",
            intercept,
            n_rows,
            design,
            coefficients
        ),
        exponential = sprintf(
            "%s ^ (%s * %s)",
            intercept,
            design,
            coefficients
        ),
        `log-linear` = sprintf(
            "exp(rep_vector(%s, %s) + %s * %s)",
            intercept,
            n_rows,
            design,
            coefficients
        )
    )
}

#' Build a covariate design matrix for new subject profiles
#'
#' Uses the fitted subject data to preserve factor levels, contrasts, and column
#' order when constructing a design matrix for population predictions.
#'
#' @param formula One-sided formula describing the subject-level covariates.
#' @param newdata Data frame containing the population profiles to predict.
#' @param reference_data Subject data used to fit the model.
#' @param argument Name of the user-facing formula argument, used in errors.
#'
#' @keywords internal
#' @returns A numeric design matrix matching the fitted design matrix.
.covariate_prediction_design_matrix <- function(
    formula,
    newdata,
    reference_data,
    argument = "formula"
) {
    variables <- all.vars(formula)
    missing_variables <- setdiff(variables, names(newdata))
    assert_that(
        length(missing_variables) == 0,
        msg = sprintf(
            "All variables in `%s` must be present in `newdata`; missing: %s",
            argument,
            paste(missing_variables, collapse = ", ")
        )
    )

    reference_frame <- stats::model.frame(
        formula,
        data = reference_data,
        na.action = stats::na.fail
    )
    sentinel_level <- ".__jmpost_unused_level__"
    for (variable in names(reference_frame)) {
        values <- reference_frame[[variable]]
        if (is.character(values)) {
            values <- factor(values)
        }
        if (is.factor(values) && nlevels(values) == 1) {
            levels(values) <- c(levels(values), sentinel_level)
        }
        reference_frame[[variable]] <- values
    }

    model_terms <- stats::terms(reference_frame)
    reference_design <- stats::model.matrix(model_terms, reference_frame)
    xlevels <- stats::.getXlevels(model_terms, reference_frame)
    new_frame <- stats::model.frame(
        model_terms,
        data = newdata,
        xlev = xlevels,
        na.action = stats::na.fail
    )
    design <- stats::model.matrix(
        model_terms,
        new_frame,
        contrasts.arg = attr(reference_design, "contrasts")
    )
    remove_columns <- colnames(design) == "(Intercept)" |
        grepl(sentinel_level, colnames(design), fixed = TRUE)
    if (any(remove_columns)) {
        design <- design[, !remove_columns, drop = FALSE]
    }
    rownames(design) <- NULL

    reference_columns <- colnames(.covariate_design_matrix(
        formula,
        reference_data,
        argument
    ))
    assert_that(
        identical(colnames(design), reference_columns),
        msg = sprintf(
            "The design matrix for `%s` does not match the fitted model",
            argument
        )
    )
    design
}

#' Apply positivity constraints to a predictor intercept prior
#'
#' @param prior A [`Prior`] object for the predictor intercept.
#' @param parametrization Character scalar naming the predictor
#'   parametrization.
#' @param positive Whether the resulting parameter must be positive even when
#'   the parametrization itself does not require a positive intercept.
#'
#' @keywords internal
#' @returns The input prior, with a positive lower limit when required.
.positive_intercept_prior <- function(
    prior,
    parametrization,
    positive = FALSE
) {
    if (parametrization == "exponential" || positive) {
        set_limits(prior, lower = getOption("jmpost.double_eps"))
    } else {
        prior
    }
}

#' Obtain a predictor's reference-level initial value
#'
#' @param prior A [`Prior`] object for the predictor intercept.
#' @param parametrization Character scalar naming the predictor
#'   parametrization.
#'
#' @keywords internal
#' @returns The median reference-level parameter value on its natural scale.
.predictor_reference_value <- function(prior, parametrization) {
    value <- median(prior)
    if (parametrization == "log-linear") {
        exp(value)
    } else if (parametrization == "exponential") {
        1
    } else {
        value
    }
}

#' Construct a random-slope model with subject-level covariates
#'
#' The intercept column generated by each formula is removed. The separately
#' specified intercept parameter is the reference-level value except for the
#' `exponential` parametrization, whose reference-level value is one.
#' `linear` uses `intercept + X beta`, `proportional` uses
#' `intercept * (1 + X beta)`, `exponential` uses
#' `intercept ^ (X beta)`, and `log-linear` uses
#' `exp(intercept + X beta)`.
#'
#' @param mu_formula,slope_mu_formula,slope_sigma_formula One-sided formulas
#'   evaluated in the subject-level data.
#' @param mu_parametrization,slope_mu_parametrization,slope_sigma_parametrization
#'   Predictor parametrizations.
#' @param mu_intercept_prior,mu_coefficients_prior Priors for the subject-intercept predictor.
#' @param slope_mu_intercept_prior,slope_mu_coefficients_prior Priors for the mean-slope predictor.
#' @param slope_sigma_intercept_prior,slope_sigma_coefficients_prior Priors for the slope-SD predictor.
#' @param sigma Prior for the observation-error standard deviation.
#' @param scaled_variance Whether to use multiplicative observation error.
#'
#' @returns A `LongitudinalRandomSlopeCov` object.
#' @export
#'
#' @examples
#' LongitudinalRandomSlopeCov()
LongitudinalRandomSlopeCov <- function(
    mu_formula = ~study,
    slope_mu_formula = ~arm,
    slope_sigma_formula = ~arm,
    mu_parametrization = "linear",
    slope_mu_parametrization = "linear",
    slope_sigma_parametrization = "log-linear",
    mu_intercept_prior = prior_normal(30, 10),
    mu_coefficients_prior = prior_normal(0, 3),
    slope_mu_intercept_prior = prior_normal(1, 3),
    slope_mu_coefficients_prior = prior_normal(0, 3),
    slope_sigma_intercept_prior = prior_normal(0, 1.5),
    slope_sigma_coefficients_prior = prior_normal(0, 1.5),
    sigma = prior_lognormal(0, 1.5),
    scaled_variance = FALSE
) {
    mu_formula <- .validate_covariate_formula(mu_formula, "mu_formula")
    slope_mu_formula <- .validate_covariate_formula(
        slope_mu_formula,
        "slope_mu_formula"
    )
    slope_sigma_formula <- .validate_covariate_formula(
        slope_sigma_formula,
        "slope_sigma_formula"
    )
    mu_parametrization <- .validate_covariate_parametrization(
        mu_parametrization,
        "mu_parametrization"
    )
    slope_mu_parametrization <- .validate_covariate_parametrization(
        slope_mu_parametrization,
        "slope_mu_parametrization"
    )
    slope_sigma_parametrization <- .validate_covariate_parametrization(
        slope_sigma_parametrization,
        "slope_sigma_parametrization"
    )

    mu_intercept_prior <- .positive_intercept_prior(
        mu_intercept_prior,
        mu_parametrization
    )
    slope_mu_intercept_prior <- .positive_intercept_prior(
        slope_mu_intercept_prior,
        slope_mu_parametrization
    )
    slope_sigma_intercept_prior <- .positive_intercept_prior(
        slope_sigma_intercept_prior,
        slope_sigma_parametrization,
        positive = slope_sigma_parametrization != "log-linear"
    )
    sigma <- set_limits(sigma, lower = getOption("jmpost.double_eps"))

    stan <- StanModule(decorated_render(
        .x = read_stan("lm-random-slope-cov/model.stan"),
        scaled_variance = scaled_variance,
        mu_predictor = .covariate_predictor_stan(
            "lm_rsc_mu",
            mu_parametrization
        ),
        slope_mu_predictor = .covariate_predictor_stan(
            "lm_rsc_slope_mu",
            slope_mu_parametrization
        ),
        slope_sigma_predictor = .covariate_predictor_stan(
            "lm_rsc_slope_sigma",
            slope_sigma_parametrization
        )
    ))

    .LongitudinalRandomSlopeCov(
        LongitudinalModel(
            name = "Random Slope with Covariates",
            stan = stan,
            scaled_variance = scaled_variance,
            parameters = ParameterList(
                Parameter(
                    name = "lm_rsc_mu_intercept",
                    prior = mu_intercept_prior
                ),
                Parameter(
                    name = "lm_rsc_mu_coefficients",
                    prior = mu_coefficients_prior,
                    size = "p_lm_rsc_mu"
                ),
                Parameter(
                    name = "lm_rsc_slope_mu_intercept",
                    prior = slope_mu_intercept_prior
                ),
                Parameter(
                    name = "lm_rsc_slope_mu_coefficients",
                    prior = slope_mu_coefficients_prior,
                    size = "p_lm_rsc_slope_mu"
                ),
                Parameter(
                    name = "lm_rsc_slope_sigma_intercept",
                    prior = slope_sigma_intercept_prior
                ),
                Parameter(
                    name = "lm_rsc_slope_sigma_coefficients",
                    prior = slope_sigma_coefficients_prior,
                    size = "p_lm_rsc_slope_sigma"
                ),
                Parameter(
                    name = "lm_rsc_sigma",
                    prior = sigma
                ),
                Parameter(
                    name = "lm_rsc_ind_rnd_slope",
                    prior = prior_init_only(prior_normal(
                        .predictor_reference_value(
                            slope_mu_intercept_prior,
                            slope_mu_parametrization
                        ),
                        .predictor_reference_value(
                            slope_sigma_intercept_prior,
                            slope_sigma_parametrization
                        )
                    )),
                    size = "n_subjects"
                )
            )
        ),
        mu_formula = mu_formula,
        slope_mu_formula = slope_mu_formula,
        slope_sigma_formula = slope_sigma_formula,
        mu_parametrization = mu_parametrization,
        slope_mu_parametrization = slope_mu_parametrization,
        slope_sigma_parametrization = slope_sigma_parametrization
    )
}

#' @export
#'
#' @returns A `StanModule` object containing the generated-quantities code.
enableGQ.LongitudinalRandomSlopeCov <- function(
    object,
    generator = NULL,
    type = NULL,
    ...
) {
    include_subject <- identical(type, "longitudinal") &&
        is(generator, "QuantityGeneratorSubject")
    include_population <- identical(type, "longitudinal") &&
        is(generator, "QuantityGeneratorPopulation")

    StanModule(decorated_render(
        .x = read_stan("lm-random-slope-cov/quantities.stan"),
        include_gq_longitudinal_idv = include_subject,
        include_gq_longitudinal_pop = include_population,
        mu_population_predictor = .covariate_predictor_stan(
            "lm_rsc_mu",
            object@mu_parametrization,
            design_prefix = "gq_lm_rsc_mu",
            n_rows = "gq_n_quant"
        ),
        slope_mu_population_predictor = .covariate_predictor_stan(
            "lm_rsc_slope_mu",
            object@slope_mu_parametrization,
            design_prefix = "gq_lm_rsc_slope_mu",
            n_rows = "gq_n_quant"
        )
    ))
}

#' @export
#'
#' @returns The longitudinal model with its link-related Stan code enabled.
enableLink.LongitudinalRandomSlopeCov <- function(object, ...) {
    object@stan <- merge(
        object@stan,
        StanModule("lm-random-slope-cov/link.stan")
    )
    object
}

#' @export
#'
#' @returns A `LinkComponent` object.
linkDSLD.LongitudinalRandomSlopeCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_dsld",
        stan = StanModule("lm-random-slope/link_dsld.stan"),
        prior = prior
    )
}

#' @export
#'
#' @returns A `LinkComponent` object.
linkIdentity.LongitudinalRandomSlopeCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_identity",
        stan = StanModule("lm-random-slope/link_identity.stan"),
        prior = prior
    )
}

#' @export
#'
#' @returns A `LinkComponent` object.
linkGrowth.LongitudinalRandomSlopeCov <- function(
    prior = prior_normal(0, 2),
    model,
    ...
) {
    LinkComponent(
        key = "link_growth",
        stan = StanModule("lm-random-slope/link_growth.stan"),
        prior = prior
    )
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.LongitudinalRandomSlopeCov <- function(object, ...) {
    c("intercept", "slope")
}

#' @rdname getRandomEffectsNames
#' @export
getRandomEffectsNames.LongitudinalRandomSlopeCov <- function(object, ...) {
    c("slope" = "lm_rsc_ind_rnd_slope")
}

#' Create longitudinal-model-specific Stan data
#'
#' @param model A [`LongitudinalModel`] object.
#' @param subject A [`DataSubject`] object.
#'
#' @keywords internal
#' @returns A named list of model-specific Stan data components.
longitudinal_model_stan_data <- function(model, subject) {
    if (is(model, "LongitudinalRandomSlopeCov")) {
        return(.random_slope_cov_stan_data(
            model,
            subject
        ))
    }
    if (is(model, "LongitudinalSteinFojoCov")) {
        return(.stein_fojo_cov_stan_data(
            model,
            subject
        ))
    }
    longitudinal_model_stan_data.default(model, subject)
}

#' @rdname longitudinal_model_stan_data
#' @keywords internal
.random_slope_cov_stan_data <- function(
    model,
    subject
) {
    subject_data <- as.data.frame(harmonise(subject))
    mu_design <- .covariate_design_matrix(
        model@mu_formula,
        subject_data,
        "mu_formula"
    )
    slope_mu_design <- .covariate_design_matrix(
        model@slope_mu_formula,
        subject_data,
        "slope_mu_formula"
    )
    slope_sigma_design <- .covariate_design_matrix(
        model@slope_sigma_formula,
        subject_data,
        "slope_sigma_formula"
    )

    list(
        p_lm_rsc_mu = ncol(mu_design),
        lm_rsc_mu_design = mu_design,
        p_lm_rsc_slope_mu = ncol(slope_mu_design),
        lm_rsc_slope_mu_design = slope_mu_design,
        p_lm_rsc_slope_sigma = ncol(slope_sigma_design),
        lm_rsc_slope_sigma_design = slope_sigma_design
    )
}

#' Create population-prediction Stan data for a covariate random-slope model
#'
#' @param model A [`LongitudinalRandomSlopeCov`] object.
#' @param subject A [`DataSubject`] object containing the fitted subject data.
#' @param newdata Population profiles supplied through [`GridPopulation()`].
#'
#' @keywords internal
#' @returns A named list of population generated-quantities design matrices.
.random_slope_cov_population_stan_data <- function(
    model,
    subject,
    newdata
) {
    subject_data <- as.data.frame(harmonise(subject))
    list(
        gq_lm_rsc_mu_design = .covariate_prediction_design_matrix(
            model@mu_formula,
            newdata,
            subject_data,
            "mu_formula"
        ),
        gq_lm_rsc_slope_mu_design = .covariate_prediction_design_matrix(
            model@slope_mu_formula,
            newdata,
            subject_data,
            "slope_mu_formula"
        )
    )
}

#' @rdname longitudinal_model_stan_data
longitudinal_model_stan_data.default <- function(model, subject) {
    list()
}
