#' @include SimLongitudinalGSF.R
#' @include LongitudinalGSFCov.R
NULL

#' Simulate a Generalized Stein-Fojo model with covariates
#'
#' @inheritParams LongitudinalGSFCov
#' @param times Observation times.
#' @param mu_b_intercept,mu_b_coefficients,omega_b_intercept,omega_b_coefficients,mu_s_intercept,mu_s_coefficients,omega_s_intercept,omega_s_coefficients,mu_g_intercept,mu_g_coefficients,omega_g_intercept,omega_g_coefficients,mu_phi_intercept,mu_phi_coefficients,omega_phi_intercept,omega_phi_coefficients Predictor coefficients.
#' @param sigma Observation-error standard deviation.
#' @param link_dsld,link_ttg,link_identity,link_growth,link_shrinkage Link coefficients.
#' @exportClass SimLongitudinalGSFCov
.SimLongitudinalGSFCov <- setClass(
    "SimLongitudinalGSFCov",
    contains = "SimLongitudinal",
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
        mu_phi_intercept = "numeric",
        mu_phi_coefficients = "numeric",
        omega_phi_intercept = "numeric",
        omega_phi_coefficients = "numeric",
        sigma = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric",
        link_shrinkage = "numeric",
        scaled_variance = "logical"
    )
)

#' @rdname SimLongitudinalGSFCov-class
#' @returns A `SimLongitudinalGSFCov` object.
#' @export
SimLongitudinalGSFCov <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
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
    mu_phi_parametrization = "linear",
    omega_phi_parametrization = "log-linear",
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
    mu_phi_intercept = qlogis(0.5),
    mu_phi_coefficients = numeric(),
    omega_phi_intercept = log(0.2),
    omega_phi_coefficients = numeric(),
    sigma = 0.01,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0,
    link_shrinkage = 0,
    scaled_variance = FALSE
) {
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
    args <- list(
        times = times,
        sigma = sigma,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth,
        link_shrinkage = link_shrinkage,
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
    do.call(.SimLongitudinalGSFCov, args)
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalGSFCov <- function(object, subjects_df) {
    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should equal the number of unique subjects"
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
    for (name in c("omega_b", "omega_s", "omega_g", "omega_phi")) {
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
            psi_s = stats::rlnorm(
                dplyr::n(),
                predicted$mu_s,
                predicted$omega_s
            ),
            psi_g = stats::rlnorm(
                dplyr::n(),
                predicted$mu_g,
                predicted$omega_g
            ),
            psi_phi = stats::plogis(stats::rnorm(
                dplyr::n(),
                predicted$mu_phi,
                predicted$omega_phi
            ))
        )
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalGSFCov <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            mu_sld = gsf_sld(
                .data$time,
                .data$psi_b,
                .data$psi_s,
                .data$psi_g,
                .data$psi_phi
            ),
            dsld = gsf_dsld(
                .data$time,
                .data$psi_b,
                .data$psi_s,
                .data$psi_g,
                .data$psi_phi
            ),
            ttg = gsf_ttg(
                .data$time,
                .data$psi_b,
                .data$psi_s,
                .data$psi_g,
                .data$psi_phi
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
                object@link_growth * log(.data$psi_g) +
                object@link_shrinkage * log(.data$psi_s)
        )
}

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalGSFCov <- function(object, ...) {
    "SimLongitudinalGSFCov"
}
