

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
#' @export
#' @keywords internal
#'
#' @examples
#' gsf_sld(1:10, 20, 0.3, 0.6, 0.2)
gsf_sld <- function(time, b, s, g, phi) {
    b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
}


#' @rdname gsf_sld
#' @export
#' @examples
#' gsf_ttg(1:10, 20, 0.3, 0.6, 0.2)
gsf_ttg <- function(time, b, s, g, phi) {
    t1 <- (log(s * phi / (g * (1 - phi))) / (g + s))
    t1[t1 <= 0] <- 0
    return(t1)
}


#' @rdname gsf_sld
#' @export
#' @examples
#' gsf_dsld(1:10, 20, 0.3, 0.6, 0.2)
gsf_dsld <- function(time, b, s, g, phi) {
    t1 <- (1 - phi) * g * exp(g * time)
    t2 <- phi * s * exp(-s * time)
    return(b * (t1 - t2))
}



#' Construct a Simulation Function for Longitudinal Data from GSF Model
#'
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param mu_s (`numeric`)\cr the mean shrinkage rates for the two treatment arms.
#' @param mu_g (`numeric`)\cr the mean growth rates for the two treatment arms.
#' @param mu_b (`numeric`)\cr the mean baseline values for the two treatment arms.
#' @param omega_b (`number`)\cr the baseline value standard deviation.
#' @param omega_s (`number`)\cr the shrinkage rate standard deviation.
#' @param omega_g (`number`)\cr the growth rate standard deviation.
#' @param a_phi (`number`)\cr the alpha parameter for the fraction of cells that respond to treatment.
#' @param b_phi (`number`)\cr the beta parameter for the fraction of cells that respond to treatment.
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param link_identity (`number`)\cr the link coefficient for the SLD Identity contribution.
#'
#' @returns A function with argument `lm_base` that can be used to simulate
#'   longitudinal data from the corresponding GSF model.
#'
#' @export
sim_lm_gsf <- function(
    sigma = 0.01,
    mu_s = c(0.6, 0.4),
    mu_g = c(0.25, 0.35),
    mu_b = 60,
    a_phi = c(4, 6),
    b_phi = c(4, 6),
    omega_b = 0.2,
    omega_s = 0.2,
    omega_g = 0.2,
    link_dsld = 0,
    link_ttg = 0,
    link_identity = 0
) {
    function(lm_base) {

        assert_that(
            length(unique(lm_base$study)) == length(mu_b),
            is.factor(lm_base$study),
            is.factor(lm_base$arm),
            length(mu_b) == 1,
            length(sigma) == 1,
            length(mu_s) == length(unique(lm_base$arm)),
            length(mu_s) == length(mu_g),
            length(mu_s) == length(a_phi),
            length(mu_s) == length(b_phi),
            length(c(omega_b, omega_s, omega_g)) == 3
        )

        baseline_covs <- lm_base |>
            dplyr::distinct(.data$pt, .data$arm, .data$study) |>
            dplyr::mutate(study_idx = as.numeric(.data$study)) |>
            dplyr::mutate(arm_idx = as.numeric(.data$arm)) |>
            dplyr::mutate(psi_b = stats::rlnorm(dplyr::n(), log(mu_b[.data$study_idx]), omega_b)) |>
            dplyr::mutate(psi_s = stats::rlnorm(dplyr::n(), log(mu_s[.data$arm_idx]), omega_s)) |>
            dplyr::mutate(psi_g = stats::rlnorm(dplyr::n(), log(mu_g[.data$arm_idx]), omega_g)) |>
            dplyr::mutate(psi_phi = stats::rbeta(dplyr::n(), a_phi[.data$arm_idx], b_phi[.data$arm_idx]))

        lm_dat <- lm_base |>
            dplyr::select(!dplyr::all_of(c("study", "arm"))) |>
            dplyr::left_join(baseline_covs, by = "pt") |>
            dplyr::mutate(mu_sld = gsf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(dsld = gsf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(ttg = gsf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * sigma)) |>
            dplyr::mutate(
                log_haz_link =
                    (link_dsld * .data$dsld) +
                    (link_ttg * .data$ttg) +
                    (link_identity * .data$mu_sld)
            )
        return(lm_dat)
    }
}
