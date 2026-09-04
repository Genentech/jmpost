#' @include SimLongitudinalSteinFojo.R
#' @include LongitudinalSteinFojoCov.R
NULL

#' Simulate a Stein-Fojo longitudinal model with covariates
#'
#' @exportClass SimLongitudinalSteinFojoCov
.SimLongitudinalSteinFojoCov <- setClass(
    "SimLongitudinalSteinFojoCov",
    contains = "SimLongitudinal",
    slots = c(
        mu_b_formula = "formula",
        omega_b_formula = "formula",
        mu_s_formula = "formula",
        omega_s_formula = "formula",
        mu_g_formula = "formula",
        omega_g_formula = "formula",
        mu_b_parametrization = "character",
        omega_b_parametrization = "character",
        mu_s_parametrization = "character",
        omega_s_parametrization = "character",
        mu_g_parametrization = "character",
        omega_g_parametrization = "character",
        mu_b_intercept = "numeric",
        mu_b_coefficients = "numeric",
        omega_b_intercept = "numeric",
        omega_b_coefficients = "numeric",
        mu_s_intercept = "numeric",
        mu_s_coefficients = "numeric",
        omega_s_intercept = "numeric",
        omega_s_coefficients = "numeric",
        mu_g_intercept = "numeric",
        mu_g_coefficients = "numeric",
        omega_g_intercept = "numeric",
        omega_g_coefficients = "numeric",
        sigma = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric",
        link_shrinkage = "numeric",
        scaled_variance = "logical"
    )
)

#' Construct a covariate Stein-Fojo simulator
#'
#' @inheritParams LongitudinalSteinFojoCov
#' @param times Observation times.
#' @param mu_b_intercept,mu_b_coefficients Coefficients for the baseline log-mean predictor.
#' @param omega_b_intercept,omega_b_coefficients Coefficients for the baseline log-SD predictor.
#' @param mu_s_intercept,mu_s_coefficients Coefficients for the shrinkage log-mean predictor.
#' @param omega_s_intercept,omega_s_coefficients Coefficients for the shrinkage log-SD predictor.
#' @param mu_g_intercept,mu_g_coefficients Coefficients for the growth log-mean predictor.
#' @param omega_g_intercept,omega_g_coefficients Coefficients for the growth log-SD predictor.
#' @param sigma Observation-error standard deviation.
#' @param link_dsld,link_ttg,link_identity,link_growth,link_shrinkage Link coefficients.
#'
#' @returns A `SimLongitudinalSteinFojoCov` object.
#' @export
SimLongitudinalSteinFojoCov <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
    mu_b_formula = ~study,
    omega_b_formula = ~study,
    mu_s_formula = ~arm,
    omega_s_formula = ~arm,
    mu_g_formula = ~arm,
    omega_g_formula = ~arm,
    mu_b_parametrization = "linear",
    omega_b_parametrization = "log-linear",
    mu_s_parametrization = "linear",
    omega_s_parametrization = "log-linear",
    mu_g_parametrization = "linear",
    omega_g_parametrization = "log-linear",
    mu_b_intercept = log(60),
    mu_b_coefficients = numeric(),
    omega_b_intercept = log(0.2),
    omega_b_coefficients = numeric(),
    mu_s_intercept = log(0.5),
    mu_s_coefficients = numeric(),
    omega_s_intercept = log(0.2),
    omega_s_coefficients = numeric(),
    mu_g_intercept = log(0.3),
    mu_g_coefficients = numeric(),
    omega_g_intercept = log(0.2),
    omega_g_coefficients = numeric(),
    sigma = 0.01,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0,
    link_shrinkage = 0,
    scaled_variance = FALSE
) {
    .SimLongitudinalSteinFojoCov(
        times = times,
        mu_b_formula = .validate_covariate_formula(mu_b_formula, "mu_b_formula"),
        omega_b_formula = .validate_covariate_formula(omega_b_formula, "omega_b_formula"),
        mu_s_formula = .validate_covariate_formula(mu_s_formula, "mu_s_formula"),
        omega_s_formula = .validate_covariate_formula(omega_s_formula, "omega_s_formula"),
        mu_g_formula = .validate_covariate_formula(mu_g_formula, "mu_g_formula"),
        omega_g_formula = .validate_covariate_formula(omega_g_formula, "omega_g_formula"),
        mu_b_parametrization = .validate_covariate_parametrization(mu_b_parametrization, "mu_b_parametrization"),
        omega_b_parametrization = .validate_covariate_parametrization(omega_b_parametrization, "omega_b_parametrization"),
        mu_s_parametrization = .validate_covariate_parametrization(mu_s_parametrization, "mu_s_parametrization"),
        omega_s_parametrization = .validate_covariate_parametrization(omega_s_parametrization, "omega_s_parametrization"),
        mu_g_parametrization = .validate_covariate_parametrization(mu_g_parametrization, "mu_g_parametrization"),
        omega_g_parametrization = .validate_covariate_parametrization(omega_g_parametrization, "omega_g_parametrization"),
        mu_b_intercept = mu_b_intercept,
        mu_b_coefficients = mu_b_coefficients,
        omega_b_intercept = omega_b_intercept,
        omega_b_coefficients = omega_b_coefficients,
        mu_s_intercept = mu_s_intercept,
        mu_s_coefficients = mu_s_coefficients,
        omega_s_intercept = omega_s_intercept,
        omega_s_coefficients = omega_s_coefficients,
        mu_g_intercept = mu_g_intercept,
        mu_g_coefficients = mu_g_coefficients,
        omega_g_intercept = omega_g_intercept,
        omega_g_coefficients = omega_g_coefficients,
        sigma = sigma,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth,
        link_shrinkage = link_shrinkage,
        scaled_variance = scaled_variance
    )
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalSteinFojoCov <- function(object, subjects_df) {
    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should equal the number of unique subjects"
    )
    parameter_names <- c("mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g")
    predicted <- lapply(parameter_names, function(name) {
        design <- .covariate_design_matrix(
            slot(object, paste0(name, "_formula")),
            subjects_df,
            paste0(name, "_formula")
        )
        coefficients <- .simulation_coefficients(
            slot(object, paste0(name, "_coefficients")),
            ncol(design),
            paste0(name, "_coefficients")
        )
        .covariate_predictor_r(
            design,
            slot(object, paste0(name, "_intercept")),
            coefficients,
            slot(object, paste0(name, "_parametrization"))
        )
    })
    names(predicted) <- parameter_names
    for (name in c("omega_b", "omega_s", "omega_g")) {
        assert_that(
            all(predicted[[name]] > 0),
            msg = sprintf("The %s predictor must be positive for every subject", name)
        )
    }

    subjects_df |>
        dplyr::mutate(
            psi_b = stats::rlnorm(dplyr::n(), predicted$mu_b, predicted$omega_b),
            psi_s = stats::rlnorm(dplyr::n(), predicted$mu_s, predicted$omega_s),
            psi_g = stats::rlnorm(dplyr::n(), predicted$mu_g, predicted$omega_g)
        )
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalSteinFojoCov <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            mu_sld = sf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g),
            dsld = sf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g),
            ttg = sf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g),
            sld_sd = ifelse(
                object@scaled_variance,
                pmax(.data$mu_sld * object@sigma, .Machine$double.eps),
                object@sigma
            ),
            sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$sld_sd),
            log_haz_link = (object@link_dsld * .data$dsld) +
                (object@link_ttg * .data$ttg) +
                (object@link_identity * .data$mu_sld) +
                (object@link_growth * log(.data$psi_g)) +
                (object@link_shrinkage * log(.data$psi_s))
        )
}

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalSteinFojoCov <- function(object, ...) {
    "SimLongitudinalSteinFojoCov"
}
