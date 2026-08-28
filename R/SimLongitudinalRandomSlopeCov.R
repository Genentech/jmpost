#' @include SimLongitudinalRandomSlope.R
#' @include LongitudinalRandomSlopeCov.R
NULL

#' Simulate a random-slope longitudinal model with covariates
#'
#' @exportClass SimLongitudinalRandomSlopeCov
.SimLongitudinalRandomSlopeCov <- setClass(
    "SimLongitudinalRandomSlopeCov",
    contains = "SimLongitudinal",
    slots = c(
        mu_formula = "formula",
        slope_mu_formula = "formula",
        slope_sigma_formula = "formula",
        mu_parametrization = "character",
        slope_mu_parametrization = "character",
        slope_sigma_parametrization = "character",
        mu_intercept = "numeric",
        mu_coefficients = "numeric",
        slope_mu_intercept = "numeric",
        slope_mu_coefficients = "numeric",
        slope_sigma_intercept = "numeric",
        slope_sigma_coefficients = "numeric",
        sigma = "numeric",
        link_dsld = "numeric",
        link_identity = "numeric",
        scaled_variance = "logical"
    )
)

#' Construct a random-slope simulator with subject-level covariates
#'
#' @inheritParams LongitudinalRandomSlopeCov
#' @param times Observation times.
#' @param mu_intercept,mu_coefficients Coefficients for the subject intercept.
#' @param slope_mu_intercept,slope_mu_coefficients Coefficients for the mean slope.
#' @param slope_sigma_intercept,slope_sigma_coefficients Coefficients for the slope SD.
#' @param sigma Observation-error standard deviation.
#' @param link_dsld,link_identity Link coefficients.
#'
#' @returns A `SimLongitudinalRandomSlopeCov` object.
#' @export
SimLongitudinalRandomSlopeCov <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550),
    mu_formula = ~study,
    slope_mu_formula = ~arm,
    slope_sigma_formula = ~arm,
    mu_parametrization = "linear",
    slope_mu_parametrization = "linear",
    slope_sigma_parametrization = "log-linear",
    mu_intercept = 50,
    mu_coefficients = numeric(),
    slope_mu_intercept = 0.01,
    slope_mu_coefficients = numeric(),
    slope_sigma_intercept = log(0.5),
    slope_sigma_coefficients = numeric(),
    sigma = 2,
    link_dsld = 0,
    link_identity = 0,
    scaled_variance = FALSE
) {
    .SimLongitudinalRandomSlopeCov(
        times = times,
        mu_formula = .validate_covariate_formula(mu_formula, "mu_formula"),
        slope_mu_formula = .validate_covariate_formula(
            slope_mu_formula,
            "slope_mu_formula"
        ),
        slope_sigma_formula = .validate_covariate_formula(
            slope_sigma_formula,
            "slope_sigma_formula"
        ),
        mu_parametrization = .validate_covariate_parametrization(
            mu_parametrization,
            "mu_parametrization"
        ),
        slope_mu_parametrization = .validate_covariate_parametrization(
            slope_mu_parametrization,
            "slope_mu_parametrization"
        ),
        slope_sigma_parametrization = .validate_covariate_parametrization(
            slope_sigma_parametrization,
            "slope_sigma_parametrization"
        ),
        mu_intercept = mu_intercept,
        mu_coefficients = mu_coefficients,
        slope_mu_intercept = slope_mu_intercept,
        slope_mu_coefficients = slope_mu_coefficients,
        slope_sigma_intercept = slope_sigma_intercept,
        slope_sigma_coefficients = slope_sigma_coefficients,
        sigma = sigma,
        link_dsld = link_dsld,
        link_identity = link_identity,
        scaled_variance = scaled_variance
    )
}

#' Normalise simulation coefficients to a design matrix
#'
#' An empty vector represents zero coefficients. For convenience, a full set of
#' dummy coefficients may be supplied when its reference-level coefficient is
#' zero; that leading zero is removed to match reference coding.
#'
#' @param coefficients Numeric vector of simulation coefficients.
#' @param n Required number of design-matrix coefficients.
#' @param argument Name of the user-facing argument, used in error messages.
#'
#' @keywords internal
#' @returns A numeric coefficient vector of length `n`.
.simulation_coefficients <- function(coefficients, n, argument) {
    if (length(coefficients) == 0) {
        return(rep(0, n))
    }
    if (length(coefficients) == n + 1 && coefficients[[1]] == 0) {
        return(coefficients[-1])
    }
    assert_that(
        length(coefficients) == n,
        msg = sprintf(
            "`%s` must have length %d (or length %d starting with zero)",
            argument,
            n,
            n + 1
        )
    )
    coefficients
}

#' Evaluate a covariate predictor in R
#'
#' @param design Numeric subject-level design matrix.
#' @param intercept Numeric scalar predictor intercept.
#' @param coefficients Numeric vector with one coefficient per design column.
#' @param parametrization Character scalar naming the predictor
#'   parametrization.
#'
#' @keywords internal
#' @returns A numeric vector with one predicted value per design row.
.covariate_predictor_r <- function(
    design,
    intercept,
    coefficients,
    parametrization
) {
    linear_term <- drop(design %*% coefficients)
    switch(parametrization,
        linear = intercept + linear_term,
        proportional = intercept * (1 + linear_term),
        exponential = intercept * exp(linear_term),
        `log-linear` = exp(intercept + linear_term)
    )
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalRandomSlopeCov <- function(object, subjects_df) {
    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should equal the number of unique subjects"
    )

    mu_design <- .covariate_design_matrix(
        object@mu_formula,
        subjects_df,
        "mu_formula"
    )
    slope_mu_design <- .covariate_design_matrix(
        object@slope_mu_formula,
        subjects_df,
        "slope_mu_formula"
    )
    slope_sigma_design <- .covariate_design_matrix(
        object@slope_sigma_formula,
        subjects_df,
        "slope_sigma_formula"
    )

    mu_coefficients <- .simulation_coefficients(
        object@mu_coefficients,
        ncol(mu_design),
        "mu_coefficients"
    )
    slope_mu_coefficients <- .simulation_coefficients(
        object@slope_mu_coefficients,
        ncol(slope_mu_design),
        "slope_mu_coefficients"
    )
    slope_sigma_coefficients <- .simulation_coefficients(
        object@slope_sigma_coefficients,
        ncol(slope_sigma_design),
        "slope_sigma_coefficients"
    )

    intercept <- .covariate_predictor_r(
        mu_design,
        object@mu_intercept,
        mu_coefficients,
        object@mu_parametrization
    )
    slope_mu <- .covariate_predictor_r(
        slope_mu_design,
        object@slope_mu_intercept,
        slope_mu_coefficients,
        object@slope_mu_parametrization
    )
    slope_sigma <- .covariate_predictor_r(
        slope_sigma_design,
        object@slope_sigma_intercept,
        slope_sigma_coefficients,
        object@slope_sigma_parametrization
    )
    assert_that(
        all(slope_sigma > 0),
        msg = "The slope standard-deviation predictor must be positive for every subject"
    )

    subjects_df |>
        dplyr::mutate(
            intercept = intercept,
            slope_ind = stats::rnorm(
                dplyr::n(),
                mean = slope_mu,
                sd = slope_sigma
            )
        )
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalRandomSlopeCov <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            sld_mu = .data$intercept + .data$slope_ind * .data$time,
            sld_sd = ifelse(
                object@scaled_variance,
                pmax(.data$sld_mu * object@sigma, .Machine$double.eps),
                object@sigma
            ),
            sld = stats::rnorm(dplyr::n(), .data$sld_mu, .data$sld_sd),
            log_haz_link = object@link_dsld * .data$slope_ind +
                object@link_identity * .data$sld_mu
        )
}

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalRandomSlopeCov <- function(object, ...) {
    "SimLongitudinalRandomSlopeCov"
}
