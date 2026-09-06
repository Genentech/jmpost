#' @include SimLongitudinalClaretBruno.R
#' @include LongitudinalClaretBrunoCov.R
NULL

#' Simulate a Claret-Bruno longitudinal model with covariates
#'
#' @inheritParams LongitudinalClaretBrunoCov
#' @param times Observation times.
#' @param mu_b_intercept,mu_b_coefficients,omega_b_intercept,omega_b_coefficients,mu_g_intercept,mu_g_coefficients,omega_g_intercept,omega_g_coefficients,mu_c_intercept,mu_c_coefficients,omega_c_intercept,omega_c_coefficients,mu_p_intercept,mu_p_coefficients,omega_p_intercept,omega_p_coefficients Predictor coefficients.
#' @param sigma Observation-error standard deviation.
#' @param link_dsld,link_ttg,link_identity,link_growth Link coefficients.
#' @exportClass SimLongitudinalClaretBrunoCov
.SimLongitudinalClaretBrunoCov <- setClass(
    "SimLongitudinalClaretBrunoCov",
    contains = "SimLongitudinal",
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
        mu_b_intercept = "numeric",
        mu_b_coefficients = "numeric",
        omega_b_intercept = "numeric",
        omega_b_coefficients = "numeric",
        mu_g_intercept = "numeric",
        mu_g_coefficients = "numeric",
        omega_g_intercept = "numeric",
        omega_g_coefficients = "numeric",
        mu_c_intercept = "numeric",
        mu_c_coefficients = "numeric",
        omega_c_intercept = "numeric",
        omega_c_coefficients = "numeric",
        mu_p_intercept = "numeric",
        mu_p_coefficients = "numeric",
        omega_p_intercept = "numeric",
        omega_p_coefficients = "numeric",
        sigma = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric",
        scaled_variance = "logical"
    )
)

#' @rdname SimLongitudinalClaretBrunoCov-class
#' @returns A `SimLongitudinalClaretBrunoCov` object.
#' @export
SimLongitudinalClaretBrunoCov <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
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
    mu_b_intercept = log(60),
    mu_b_coefficients = numeric(),
    omega_b_intercept = log(0.2),
    omega_b_coefficients = numeric(),
    mu_g_intercept = log(1),
    mu_g_coefficients = numeric(),
    omega_g_intercept = log(0.2),
    omega_g_coefficients = numeric(),
    mu_c_intercept = log(0.4),
    mu_c_coefficients = numeric(),
    omega_c_intercept = log(0.2),
    omega_c_coefficients = numeric(),
    mu_p_intercept = log(2),
    mu_p_coefficients = numeric(),
    omega_p_intercept = log(0.2),
    omega_p_coefficients = numeric(),
    sigma = 0.01,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0,
    scaled_variance = FALSE
) {
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
    args <- list(
        times = times,
        sigma = sigma,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth,
        scaled_variance = scaled_variance
    )
    for (name in names) {
        args[[paste0(name, "_formula")]] <- .validate_covariate_formula(
            get(paste0(name, "_formula")),
            paste0(name, "_formula")
        )
        args[[paste0(
            name,
            "_parametrization"
        )]] <- .validate_covariate_parametrization(
            get(paste0(name, "_parametrization")),
            paste0(name, "_parametrization")
        )
        args[[paste0(name, "_intercept")]] <- get(paste0(name, "_intercept"))
        args[[paste0(name, "_coefficients")]] <- get(paste0(
            name,
            "_coefficients"
        ))
    }
    do.call(.SimLongitudinalClaretBrunoCov, args)
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalClaretBrunoCov <- function(object, subjects_df) {
    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should equal the number of unique subjects"
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
    predicted <- setNames(
        lapply(names, function(name) {
            design <- .covariate_design_matrix(
                slot(object, paste0(name, "_formula")),
                subjects_df,
                paste0(name, "_formula")
            )
            .covariate_predictor_r(
                design,
                slot(object, paste0(name, "_intercept")),
                .simulation_coefficients(
                    slot(object, paste0(name, "_coefficients")),
                    ncol(design),
                    paste0(name, "_coefficients")
                ),
                slot(object, paste0(name, "_parametrization"))
            )
        }),
        names
    )
    for (name in c("omega_b", "omega_g", "omega_c", "omega_p")) {
        assert_that(
            all(predicted[[name]] > 0),
            msg = sprintf(
                "The %s predictor must be positive for every subject",
                name
            )
        )
    }
    subjects_df |>
        dplyr::mutate(
            psi_b = stats::rlnorm(
                dplyr::n(),
                predicted$mu_b,
                predicted$omega_b
            ),
            psi_g = stats::rlnorm(
                dplyr::n(),
                predicted$mu_g,
                predicted$omega_g
            ),
            psi_c = stats::rlnorm(
                dplyr::n(),
                predicted$mu_c,
                predicted$omega_c
            ),
            psi_p = stats::rlnorm(dplyr::n(), predicted$mu_p, predicted$omega_p)
        )
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalClaretBrunoCov <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            mu_sld = clbr_sld(
                .data$time,
                .data$psi_b,
                .data$psi_g,
                .data$psi_c,
                .data$psi_p
            ),
            dsld = clbr_dsld(
                .data$time,
                .data$psi_b,
                .data$psi_g,
                .data$psi_c,
                .data$psi_p
            ),
            ttg = clbr_ttg(
                .data$time,
                .data$psi_b,
                .data$psi_g,
                .data$psi_c,
                .data$psi_p
            ),
            sld_sd = ifelse(
                object@scaled_variance,
                pmax(.data$mu_sld * object@sigma, .Machine$double.eps),
                object@sigma
            ),
            sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$sld_sd),
            log_haz_link = object@link_dsld *
                .data$dsld +
                object@link_ttg * .data$ttg +
                object@link_identity * .data$mu_sld +
                object@link_growth * log(.data$psi_g)
        )
}

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalClaretBrunoCov <- function(object, ...) {
    "SimLongitudinalClaretBrunoCov"
}
