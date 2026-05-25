#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a Claret-Bruno Model
#'
#' @typed times: numeric
#'   the times to generate observations at.
#' @typed sigma: number
#'   the variance of the longitudinal values.
#'
#' @typed mu_b: numeric
#'   the mean population baseline sld value.
#' @typed mu_g: numeric
#'   the mean population growth rate.
#' @typed mu_c: numeric
#'   the mean population resistance rate.
#' @typed mu_p: numeric
#'   the mean population growth inhibition.
#'
#' @typed omega_b: number
#'   the population standard deviation for the baseline sld value.
#' @typed omega_g: number
#'   the population standard deviation for the growth rate.
#' @typed omega_c: number
#'   the population standard deviation for the resistance rate.
#' @typed omega_p: number
#'   the population standard deviation for the growth inhibition.
#'
#' @typed link_dsld: number
#'   the link coefficient for the derivative contribution.
#' @typed link_ttg: number
#'   the link coefficient for the time-to-growth contribution.
#' @typed link_identity: number
#'   the link coefficient for the SLD Identity contribution.
#' @typed link_growth: number
#'   the link coefficient for the growth parameter contribution.
#'
#' @typed scaled_variance: logical
#'   whether the variance should be scaled by the expected value
#'   (see the "Statistical Specifications" vignette for more details)
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
#' @slot scaled_variance (`logical`)\cr See arguments.
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
        link_growth = "numeric",
        scaled_variance = "logical"
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
    link_growth = 0,
    scaled_variance = TRUE
) {
    if (length(omega_b) == 1) {
        omega_b <- rep(omega_b, length(mu_b))
    }
    if (length(omega_g) == 1) {
        omega_g <- rep(omega_g, length(mu_g))
    }
    if (length(omega_c) == 1) {
        omega_c <- rep(omega_c, length(mu_c))
    }
    if (length(omega_p) == 1) {
        omega_p <- rep(omega_p, length(mu_p))
    }

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
        link_growth = link_growth,
        scaled_variance = scaled_variance
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
            return(
                "The parameters `mu_g`, `mu_c` & `mu_p` must have the same length."
            )
        }
        pairs <- list(
            "omega_b" = "mu_b",
            "omega_g" = "mu_g",
            "omega_c" = "mu_c",
            "omega_p" = "mu_p"
        )
        for (i in seq_along(pairs)) {
            omega <- slot(object, names(pairs)[[i]])
            mu <- slot(object, pairs[[i]])
            if (!(length(omega) == length(mu))) {
                return(
                    sprintf(
                        "`%s` must be length 1 or the same length as `%s`",
                        omega,
                        mu
                    )
                )
            }
        }
        len_1_pars <- c(
            "sigma",
            "link_dsld",
            "link_ttg",
            "link_identity",
            "link_growth"
        )
        for (par in len_1_pars) {
            if (length(slot(object, par)) != 1) {
                return(sprintf(
                    "The `%s` parameter must be a length 1 numeric.",
                    par
                ))
            }
        }
        return(TRUE)
    }
)

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalClaretBruno <- function(object, ...) {
    return("SimLongitudinalClaretBruno")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalClaretBruno <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            mu_sld = clbr_sld(
                .data$time,
                .data$ind_b,
                .data$ind_g,
                .data$ind_c,
                .data$ind_p
            ),
            dsld = clbr_dsld(
                .data$time,
                .data$ind_b,
                .data$ind_g,
                .data$ind_c,
                .data$ind_p
            ),
            ttg = clbr_ttg(
                .data$time,
                .data$ind_b,
                .data$ind_g,
                .data$ind_c,
                .data$ind_p
            ),
            sld_sd = ifelse(
                object@scaled_variance,
                .data$mu_sld * object@sigma,
                object@sigma
            ),
            sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$sld_sd),

            log_haz_link = (object@link_dsld * .data$dsld) +
                (object@link_ttg * .data$ttg) +
                (object@link_identity * .data$mu_sld) +
                (object@link_growth * log(.data$ind_g))
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
        dplyr::mutate(
            ind_b = stats::rlnorm(
                dplyr::n(),
                object@mu_b[.data$study_idx],
                object@omega_b[.data$study_idx]
            )
        ) |>
        dplyr::mutate(
            ind_g = stats::rlnorm(
                dplyr::n(),
                object@mu_g[.data$arm_idx],
                object@omega_g[.data$arm_idx]
            )
        ) |>
        dplyr::mutate(
            ind_c = stats::rlnorm(
                dplyr::n(),
                object@mu_c[.data$arm_idx],
                object@omega_c[.data$arm_idx]
            )
        ) |>
        dplyr::mutate(
            ind_p = stats::rlnorm(
                dplyr::n(),
                object@mu_p[.data$arm_idx],
                object@omega_p[.data$arm_idx]
            )
        )

    res[, c("subject", "arm", "study", "ind_b", "ind_g", "ind_c", "ind_p")]
}


#' Claret-Bruno Functionals
#'
#' @typed t: numeric
#'   time grid.
#' @typed b: number
#'   baseline sld.
#' @typed g: number
#'   growth rate.
#' @typed c: number
#'   resistance rate.
#' @typed p: number
#'   growth inhibition.
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
