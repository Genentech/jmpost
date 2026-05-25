#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a GSF Model
#'
#' @typed times: numeric
#'   the times to generate observations at.
#' @typed sigma: number
#'   the variance of the longitudinal values.
#' @typed mu_s: numeric
#'   the mean shrinkage rates.
#' @typed mu_g: numeric
#'   the mean growth rates.
#' @typed mu_b: numeric
#'   the mean baseline values.
#' @typed mu_phi: numeric
#'   the mean proportion of cells affected by the treatment
#' @typed omega_b: number
#'   the baseline value standard deviation.
#' @typed omega_s: number
#'   the shrinkage rate standard deviation.
#' @typed omega_g: number
#'   the growth rate standard deviation.
#' @typed omega_phi: number
#'   for the standard deviation of the proportion of cells
#'   affected by the treatment `omega_phi`.
#' @typed link_dsld: number
#'   the link coefficient for the derivative contribution.
#' @typed link_ttg: number
#'   the link coefficient for the time-to-growth contribution.
#' @typed link_identity: number
#'   the link coefficient for the SLD Identity contribution.
#' @typed link_growth: number
#'   the link coefficient for the log-growth parameter contribution.
#' @typed link_shrinkage: number
#'   the link coefficient for the log-shrinkage parameter contribution.
#' @typed scaled_variance: logical
#'   whether the variance should be scaled by the expected value
#'   (see the "Statistical Specifications" vignette for more details)
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
#' @slot link_shrinkage (`numeric`)\cr See arguments.
#' @slot scaled_variance (`numeric`)\cr See arguments.
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
        link_growth = "numeric",
        link_shrinkage = "numeric",
        scaled_variance = "logical"
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
    link_growth = 0,
    link_shrinkage = 0,
    scaled_variance = TRUE
) {
    if (length(omega_b) == 1) {
        omega_b <- rep(omega_b, length(mu_b))
    }
    if (length(omega_s) == 1) {
        omega_s <- rep(omega_s, length(mu_s))
    }
    if (length(omega_g) == 1) {
        omega_g <- rep(omega_g, length(mu_g))
    }
    if (length(omega_phi) == 1) {
        omega_phi <- rep(omega_phi, length(mu_phi))
    }

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
        link_growth = link_growth,
        link_shrinkage = link_shrinkage,
        scaled_variance = scaled_variance
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
            return(
                "The parameters `mu_s`, `mu_g` and `mu_phi` must have the same length."
            )
        }

        pairs <- list(
            "omega_b" = "mu_b",
            "omega_s" = "mu_s",
            "omega_g" = "mu_g",
            "omega_phi" = "mu_phi"
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
            "link_growth",
            "link_shrinkage"
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
as_print_string.SimLongitudinalGSF <- function(object, ...) {
    return("SimLongitudinalGSF")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalGSF <- function(object, times_df) {
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
                .data$mu_sld * object@sigma,
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
        dplyr::mutate(
            study_idx = as.numeric(.data$study),
            arm_idx = as.numeric(.data$arm),
            psi_b = stats::rlnorm(
                dplyr::n(),
                object@mu_b[.data$study_idx],
                object@omega_b[.data$study_idx]
            ),
            psi_s = stats::rlnorm(
                dplyr::n(),
                object@mu_s[.data$arm_idx],
                object@omega_s[.data$arm_idx]
            ),
            psi_g = stats::rlnorm(
                dplyr::n(),
                object@mu_g[.data$arm_idx],
                object@omega_g[.data$arm_idx]
            ),
            psi_phi_logit = stats::rnorm(
                dplyr::n(),
                object@mu_phi[.data$arm_idx],
                object@omega_phi[.data$arm_idx]
            ),
            psi_phi = stats::plogis(.data$psi_phi_logit)
        )
    res[, c("subject", "arm", "study", "psi_b", "psi_s", "psi_g", "psi_phi")]
}


## sim_lm_gsf ----

#' Generalized Stein-Fojo Functionals
#'
#' @typed time: numeric
#'   time grid.
#' @typed b: number
#'   baseline.
#' @typed s: number
#'   shrinkage.
#' @typed g: number
#'   growth.
#' @typed phi: number
#'   shrinkage proportion.
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
