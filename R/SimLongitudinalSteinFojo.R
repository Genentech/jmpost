
#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a Stein-Fojo Model
#'
#' @param times (`numeric`)\cr the times to generate observations at.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param mu_s (`numeric`)\cr the mean shrinkage rates for the two treatment arms.
#' @param mu_g (`numeric`)\cr the mean growth rates for the two treatment arms.
#' @param mu_b (`numeric`)\cr the mean baseline values for the two treatment arms.
#' @param omega_b (`number`)\cr the baseline value standard deviation.
#' @param omega_s (`number`)\cr the shrinkage rate standard deviation.
#' @param omega_g (`number`)\cr the growth rate standard deviation.
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param link_identity (`number`)\cr the link coefficient for the SLD Identity contribution.
#' @param link_growth (`number`)\cr the link coefficient for the growth parameter contribution.
#'
#' @slot sigma (`numeric`)\cr See arguments.
#' @slot mu_s (`numeric`)\cr See arguments.
#' @slot mu_g (`numeric`)\cr See arguments.
#' @slot mu_b (`numeric`)\cr See arguments.
#' @slot omega_b (`numeric`)\cr See arguments.
#' @slot omega_s (`numeric`)\cr See arguments.
#' @slot omega_g (`numeric`)\cr See arguments.
#' @slot link_dsld (`numeric`)\cr See arguments.
#' @slot link_ttg (`numeric`)\cr See arguments.
#' @slot link_identity (`numeric`)\cr See arguments.
#' @slot link_growth (`numeric`)\cr See arguments.
#'
#' @family SimLongitudinal
#' @name SimLongitudinalSteinFojo-class
#' @exportClass SimLongitudinalSteinFojo
.SimLongitudinalSteinFojo <- setClass(
    "SimLongitudinalSteinFojo",
    contains = "SimLongitudinal",
    slots = c(
        sigma = "numeric",
        mu_s = "numeric",
        mu_g = "numeric",
        mu_b = "numeric",
        omega_b = "numeric",
        omega_s = "numeric",
        omega_g = "numeric",
        link_dsld = "numeric",
        link_ttg = "numeric",
        link_identity = "numeric",
        link_growth = "numeric"
    )
)

#' @rdname SimLongitudinalSteinFojo-class
#' @export
SimLongitudinalSteinFojo <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550) / 365,
    sigma = 0.01,
    mu_s = log(c(0.6, 0.4)),
    mu_g = log(c(0.25, 0.35)),
    mu_b = log(60),
    omega_b = 0.2,
    omega_s = 0.2,
    omega_g = 0.2,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0,
    link_growth = 0
) {
    .SimLongitudinalSteinFojo(
        times = times,
        sigma = sigma,
        mu_s = mu_s,
        mu_g = mu_g,
        mu_b = mu_b,
        omega_b = omega_b,
        omega_s = omega_s,
        omega_g = omega_g,
        link_dsld = link_dsld,
        link_ttg = link_ttg,
        link_identity = link_identity,
        link_growth = link_growth
    )
}


setValidity(
    "SimLongitudinalSteinFojo",
    function(object) {
        par_lengths <- c(
            length(object@mu_s),
            length(object@mu_g)
        )
        if (length(unique(par_lengths)) != 1) {
            return("The parameters `mu_s` and `mu_g` must have the same length.")
        }
        len_1_pars <- c(
            "sigma", "omega_b", "omega_s", "omega_g",
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
as_print_string.SimLongitudinalSteinFojo <- function(object) {
    return("SimLongitudinalSteinFojo")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalSteinFojo <- function(object, times_df) {
    times_df |>
        dplyr::mutate(mu_sld = sf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
        dplyr::mutate(dsld = sf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
        dplyr::mutate(ttg = sf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
        dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * object@sigma)) |>
        dplyr::mutate(
            log_haz_link =
                (object@link_dsld * .data$dsld) +
                (object@link_ttg * .data$ttg) +
                (object@link_identity * .data$mu_sld) +
                (object@link_growth * .data$psi_g)
        )
}


#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalSteinFojo <- function(object, subjects_df) {
    assert_that(
        is.factor(subjects_df$study),
        is.factor(subjects_df$arm),
        length(levels(subjects_df$study)) == length(object@mu_b),
        length(levels(subjects_df$arm)) == length(object@mu_s)
    )

    res <- subjects_df |>
        dplyr::distinct(.data$subject, .data$arm, .data$study) |>
        dplyr::mutate(study_idx = as.numeric(.data$study)) |>
        dplyr::mutate(arm_idx = as.numeric(.data$arm)) |>
        dplyr::mutate(psi_b = stats::rlnorm(dplyr::n(), object@mu_b[.data$study_idx], object@omega_b)) |>
        dplyr::mutate(psi_s = stats::rlnorm(dplyr::n(), object@mu_s[.data$arm_idx], object@omega_s)) |>
        dplyr::mutate(psi_g = stats::rlnorm(dplyr::n(), object@mu_g[.data$arm_idx], object@omega_g))

    res[, c("subject", "arm", "study", "psi_b", "psi_s", "psi_g")]
}


#' Stein-Fojo Functionals
#'
#' @param time (`numeric`)\cr time grid.
#' @param b (`number`)\cr baseline.
#' @param s (`number`)\cr shrinkage.
#' @param g (`number`)\cr growth.
#'
#' @returns The function results.
#' @keywords internal
sf_sld <- function(time, b, s, g) {
    s <- dplyr::if_else(time >= 0, s, 0)
    b * (exp(-s * time) + exp(g * time) - 1)
}


#' @rdname sf_sld
sf_ttg <- function(time, b, s, g) {
    t1 <- (log(s) - log(g)) / (g + s)
    t1[t1 <= 0] <- 0
    return(t1)
}


#' @rdname sf_sld
sf_dsld <- function(time, b, s, g) {
    s <- dplyr::if_else(time >= 0, s, 0)
    t1 <- g * exp(g * time)
    t2 <- s * exp(-s * time)
    return(b * (t1 - t2))
}
