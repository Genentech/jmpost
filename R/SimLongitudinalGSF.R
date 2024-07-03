

#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a GSF Model
#'
#' @param times (`numeric`)\cr the times to generate observations at.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param mu_s (`numeric`)\cr the mean shrinkage rates.
#' @param mu_g (`numeric`)\cr the mean growth rates.
#' @param mu_b (`numeric`)\cr the mean baseline values.
#' @param mu_phi (`numeric`)\cr the mean proportion of cells affected by the treatment
#' @param omega_b (`number`)\cr the baseline value standard deviation.
#' @param omega_s (`number`)\cr the shrinkage rate standard deviation.
#' @param omega_g (`number`)\cr the growth rate standard deviation.
#' @param omega_phi (`number`)\cr for the standard deviation of the proportion of cells
#' affected by the treatment `omega_phi`.
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param link_identity (`number`)\cr the link coefficient for the SLD Identity contribution.
#' @param link_growth (`number`)\cr the link coefficient for the growth parameter contribution.
#'
#' @slot sigma (`numeric`)\cr See arguments.
#' @slot mu_s (`numeric`)\cr See arguments.
#' @slot mu_g (`numeric`)\cr See arguments.
#' @slot mu_b (`numeric`)\cr See arguments.
#' @slot mu_phi (`numeric`)\cr See arguments.
#' @slot omega_b (`numeric`)\cr See arguments.
#' @slot omega_s (`numeric`)\cr See arguments.
#' @slot omega_g (`numeric`)\cr See arguments.
#' @slot omega_phi (`numeric`)\cr See arguments.
#' @slot link_dsld (`numeric`)\cr See arguments.
#' @slot link_ttg (`numeric`)\cr See arguments.
#' @slot link_identity (`numeric`)\cr See arguments.
#' @slot link_growth (`numeric`)\cr See arguments.
#' @family SimLongitudinal
#' @name SimLongitudinalGSF-class
#' @exportClass SimLongitudinalGSF
.SimLongitudinalGSF <- setClass(
    "SimLongitudinalGSF",
    contains = "SimLongitudinal",
    slots = c(
        sigma = "numeric",
        mu_s = "numeric",
        mu_g = "numeric",
        mu_b = "numeric",
        mu_phi = "numeric",
        omega_b = "numeric",
        omega_s = "numeric",
        omega_g = "numeric",
        omega_phi = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric"
    )
)

#' @rdname SimLongitudinalGSF-class
#' @export
SimLongitudinalGSF <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
    sigma = 0.01,
    mu_s = log(c(0.6, 0.4)),
    mu_g = log(c(0.25, 0.35)),
    mu_b = log(60),
    mu_phi = qlogis(c(0.4, 0.6)),
    omega_b = 0.2,
    omega_s = 0.2,
    omega_g = 0.2,
    omega_phi = 0.2,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0
) {
    .SimLongitudinalGSF(
        times = times,
        sigma = sigma,
        mu_s = mu_s,
        mu_g = mu_g,
        mu_b = mu_b,
        mu_phi = mu_phi,
        omega_b = omega_b,
        omega_s = omega_s,
        omega_g = omega_g,
        omega_phi = omega_phi,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth
    )
}


setValidity(
    "SimLongitudinalGSF",
    function(object) {
        par_lengths <- c(
            length(object@mu_s),
            length(object@mu_g),
            length(object@mu_phi)
        )
        if (length(unique(par_lengths)) != 1) {
            return("The parameters `mu_s`, `mu_g` and `mu_phi` must have the same length.")
        }

        len_1_pars <- c(
            "sigma", "omega_b", "omega_s", "omega_g", "omega_phi",
            "link_dsld", "link_ttg", "link_identity", "link_growth"
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
as_print_string.SimLongitudinalGSF <- function(object) {
    return("SimLongitudinalGSF")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalGSF <- function(object, times_df) {
    times_df |>
        dplyr::mutate(mu_sld = gsf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
        dplyr::mutate(dsld = gsf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
        dplyr::mutate(ttg = gsf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
        dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * object@sigma)) |>
        dplyr::mutate(
            log_haz_link =
                (object@link_dsld * .data$dsld) +
                (object@link_ttg * .data$ttg) +
                (object@link_identity * .data$mu_sld) +
                (object@link_growth * log(.data$psi_g))
        )
}


#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalGSF <- function(object, subjects_df) {
    assert_that(
        is.factor(subjects_df$study),
        is.factor(subjects_df$arm),
        length(levels(subjects_df$study)) == length(object@mu_b),
        length(levels(subjects_df$arm)) == length(object@mu_s),
        length(levels(subjects_df$arm)) == length(object@mu_g),
        length(levels(subjects_df$arm)) == length(object@mu_phi)
    )

    res <- subjects_df |>
        dplyr::distinct(.data$subject, .data$arm, .data$study) |>
        dplyr::mutate(study_idx = as.numeric(.data$study)) |>
        dplyr::mutate(arm_idx = as.numeric(.data$arm)) |>
        dplyr::mutate(psi_b = stats::rlnorm(dplyr::n(), object@mu_b[.data$study_idx], object@omega_b)) |>
        dplyr::mutate(psi_s = stats::rlnorm(dplyr::n(), object@mu_s[.data$arm_idx], object@omega_s)) |>
        dplyr::mutate(psi_g = stats::rlnorm(dplyr::n(), object@mu_g[.data$arm_idx], object@omega_g)) |>
        dplyr::mutate(psi_phi_logit = stats::rnorm(
            dplyr::n(),
            object@mu_phi[.data$arm_idx],
            object@omega_phi
        )) |>
        dplyr::mutate(psi_phi = stats::plogis(psi_phi_logit))

    res[, c("subject", "arm", "study", "psi_b", "psi_s", "psi_g", "psi_phi")]
}




## sim_lm_gsf ----

#' Generalized Stein-Fojo Functionals
#'
#' @param time (`numeric`)\cr time grid.
#' @param b (`number`)\cr baseline.
#' @param s (`number`)\cr shrinkage.
#' @param g (`number`)\cr growth.
#' @param phi (`number`)\cr shrinkage proportion.
#'
#' @returns The function results.
#'
#' @keywords internal
gsf_sld <- function(time, b, s, g, phi) {
    phi <- dplyr::if_else(time >= 0, phi, 0)
    b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
}


#' @rdname gsf_sld
gsf_ttg <- function(time, b, s, g, phi) {
    t1 <- (log(s * phi / (g * (1 - phi))) / (g + s))
    t1[t1 <= 0] <- 0
    return(t1)
}


#' @rdname gsf_sld
gsf_dsld <- function(time, b, s, g, phi) {
    phi <- dplyr::if_else(time >= 0, phi, 0)
    t1 <- (1 - phi) * g * exp(g * time)
    t2 <- phi * s * exp(-s * time)
    return(b * (t1 - t2))
}
