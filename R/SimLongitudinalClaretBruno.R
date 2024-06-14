
#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a Claret-Bruno Model
#'
#' @param times (`numeric`)\cr the times to generate observations at.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#'
#' @param mu_b (`numeric`)\cr the mean population baseline sld value.
#' @param mu_g (`numeric`)\cr the mean population growth rate.
#' @param mu_c (`numeric`)\cr the mean population resistance rate.
#' @param mu_p (`numeric`)\cr the mean population growth inhibition.
#'
#' @param omega_b (`number`)\cr the population standard deviation for the baseline sld value.
#' @param omega_g (`number`)\cr the population standard deviation for the growth rate.
#' @param omega_c (`number`)\cr the population standard deviation for the resistance rate.
#' @param omega_p (`number`)\cr the population standard deviation for the growth inhibition.
#'
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param link_identity (`number`)\cr the link coefficient for the SLD Identity contribution.
#' @param link_growth (`number`)\cr the link coefficient for the growth parameter contribution.
#'
#' @slot sigma (`numeric`)\cr See arguments.
#'
#' @slot mu_b (`numeric`)\cr See arguments.
#' @slot mu_g (`numeric`)\cr See arguments.
#' @slot mu_c (`numeric`)\cr See arguments.
#' @slot mu_p (`numeric`)\cr See arguments.
#'
#' @slot omega_b (`numeric`)\cr See arguments.
#' @slot omega_g (`numeric`)\cr See arguments.
#' @slot omega_c (`numeric`)\cr See arguments.
#' @slot omega_p (`numeric`)\cr See arguments.
#'
#' @slot link_dsld (`numeric`)\cr See arguments.
#' @slot link_ttg (`numeric`)\cr See arguments.
#' @slot link_identity (`numeric`)\cr See arguments.
#' @slot link_growth (`numeric`)\cr See arguments.
#'
#' @family SimLongitudinal
#' @name SimLongitudinalClaretBruno-class
#' @exportClass SimLongitudinalClaretBruno
.SimLongitudinalClaretBruno <- setClass(
    "SimLongitudinalClaretBruno",
    contains = "SimLongitudinal",
    slots = c(
        sigma = "numeric",
        mu_b = "numeric",
        mu_g = "numeric",
        mu_c = "numeric",
        mu_p = "numeric",
        omega_b = "numeric",
        omega_g = "numeric",
        omega_c = "numeric",
        omega_p = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric"
    )
)

#' @rdname SimLongitudinalClaretBruno-class
#' @export
SimLongitudinalClaretBruno <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
    sigma = 0.01,
    mu_b = log(60),
    mu_g = log(c(0.9, 1.1)),
    mu_c = log(c(0.25, 0.35)),
    mu_p = log(c(1.5, 2)),
    omega_b = 0.2,
    omega_g = 0.2,
    omega_c = 0.2,
    omega_p = 0.2,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0
) {
    .SimLongitudinalClaretBruno(
        times = times,
        sigma = sigma,
        mu_b = mu_b,
        mu_g = mu_g,
        mu_c = mu_c,
        mu_p = mu_p,
        omega_b = omega_b,
        omega_g = omega_g,
        omega_c = omega_c,
        omega_p = omega_p,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth
    )
}


setValidity(
    "SimLongitudinalClaretBruno",
    function(object) {
        par_lengths <- c(
            length(object@mu_g),
            length(object@mu_c),
            length(object@mu_p)
        )
        if (length(unique(par_lengths)) != 1) {
            return("The parameters `mu_g`, `mu_c` & `mu_p` must have the same length.")
        }
        len_1_pars <- c(
            "sigma", "omega_b", "omega_g", "omega_c", "omega_p",
            "link_dsld", "link_ttg", "link_identity",
            "link_growth"
        )
        for (par in len_1_pars) {
            if (length(slot(object, par)) != 1) {
                return(sprintf("The `%s` parameter must be a length 1 numeric.", par))
            }
        }
        return(TRUE)
    }
)

#' @rdname as_print_string
as_print_string.SimLongitudinalClaretBruno <- function(object) {
    return("SimLongitudinalClaretBruno")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalClaretBruno <- function(object, times_df) {
    times_df |>
        dplyr::mutate(mu_sld = clbr_sld(.data$time, .data$ind_b, .data$ind_g, .data$ind_c, .data$ind_p)) |>
        dplyr::mutate(dsld = clbr_dsld(.data$time, .data$ind_b, .data$ind_g, .data$ind_c, .data$ind_p)) |>
        dplyr::mutate(ttg = clbr_ttg(.data$time, .data$ind_b, .data$ind_g, .data$ind_c, .data$ind_p)) |>
        dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * object@sigma)) |>
        dplyr::mutate(
            log_haz_link =
                (object@link_dsld * .data$dsld) +
                (object@link_ttg * .data$ttg) +
                (object@link_identity * .data$mu_sld) +
                (object@link_growth * .data$ind_g)
        )
}


#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalClaretBruno <- function(object, subjects_df) {
    assert_that(
        is.factor(subjects_df$study),
        is.factor(subjects_df$arm),
        length(levels(subjects_df$study)) == length(object@mu_b),
        length(levels(subjects_df$arm)) == length(object@mu_g),
        length(levels(subjects_df$arm)) == length(object@mu_c),
        length(levels(subjects_df$arm)) == length(object@mu_p)
    )

    res <- subjects_df |>
        dplyr::distinct(.data$subject, .data$arm, .data$study) |>
        dplyr::mutate(study_idx = as.numeric(.data$study)) |>
        dplyr::mutate(arm_idx = as.numeric(.data$arm)) |>
        dplyr::mutate(ind_b = stats::rlnorm(dplyr::n(), object@mu_b[.data$study_idx], object@omega_b)) |>
        dplyr::mutate(ind_g = stats::rlnorm(dplyr::n(), object@mu_g[.data$arm_idx], object@omega_g)) |>
        dplyr::mutate(ind_c = stats::rlnorm(dplyr::n(), object@mu_c[.data$arm_idx], object@omega_c)) |>
        dplyr::mutate(ind_p = stats::rlnorm(dplyr::n(), object@mu_p[.data$arm_idx], object@omega_p))

    res[, c("subject", "arm", "study", "ind_b", "ind_g", "ind_c", "ind_p")]
}


#' Claret-Bruno Functionals
#'
#' @param time (`numeric`)\cr time grid.
#' @param b (`number`)\cr baseline sld.
#' @param g (`number`)\cr growth rate.
#' @param c (`number`)\cr resistance rate.
#' @param p (`number`)\cr growth inhibition.
#'
#' @returns The function results.
#' @keywords internal
clbr_sld <- function(t, b, g, c, p) {
    p <- ifelse(t >= 0, p, 0)
    b * exp((g * t) - (p / c) * (1 - exp(-c * t)))
}

#' @rdname clbr_sld
clbr_ttg <- function(t, b, g, c, p) {
    log(p / g) / c
}

#' @rdname clbr_sld
clbr_dsld <- function(t, b, g, c, p) {
    lt0 <- b * g * exp(g * t)
    gt0 <- (g - p * exp(-c * t)) * clbr_sld(t, b, g, c, p)
    ifelse(t >= 0, gt0, lt0)
}
