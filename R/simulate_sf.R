



#' Stein-Fojo Functionals
#'
#' @param time (`numeric`)\cr time grid.
#' @param b (`number`)\cr baseline.
#' @param s (`number`)\cr shrinkage.
#' @param g (`number`)\cr growth.
#'
#' @returns The function results.
#' @export
#' @keywords internal
#'
#' @examples
#' sf_sld(1:10, 20, 0.3, 0.6)
sf_sld <- function(time, b, s, g) {
    b * (exp(-s * time) +  exp(g * time) - 1)
}

#' @rdname sf_sld
#' @export
#' @examples
#' sf_ttg(1:10, 20, 0.3, 0.6)
sf_ttg <- function(time, b, s, g) {
    (log(s) - log(g)) / (s + g)
}


#' @rdname sf_sld
#' @export
#' @examples
#' sf_dsld(1:10, 20, 0.3, 0.6)
sf_dsld <- function(time, b, s, g) {
    b * (g * exp(g * time) - s * exp(-s * time))
}


#' Construct a Simulation Function for Longitudinal Data from SF Model
#'
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param mu_s (`numeric`)\cr the mean shrinkage rates for the two treatment arms.
#' @param mu_g (`numeric`)\cr the mean growth rates for the two treatment arms.
#' @param mu_b (`numeric`)\cr the mean baseline values for the two treatment arms.
#' @param omega_b (`number`)\cr the baseline value standard deviation.
#' @param omega_s (`number`)\cr the shrinkage rate standard deviation.
#' @param omega_g (`number`)\cr the growth rate standard deviation.
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param .debug (`flag`)\cr whether to enter debug mode such that the function
#'   would only return a subset of columns.
#'
#' @returns A function with argument `lm_base` that can be used to simulate
#'   longitudinal data from the corresponding SF model.
#'
#' @export
sim_lm_sf <- function(
    sigma = 0.01,
    mu_s = c(3, 4),
    mu_g = c(0.2, 0.3),
    mu_b = 50,
    omega_b = 0.135,
    omega_s = 0.15,
    omega_g = 0.225,
    link_dsld = 0,
    link_ttg = 0,
    .debug = FALSE
) {
    function(lm_base) {

        assert_that(
            length(unique(lm_base$study)) == 1,
            length(mu_b) == 1,
            length(sigma) == 1,
            length(mu_s) == length(unique(lm_base$arm)),
            length(mu_s) == length(mu_g),
            length(c(omega_b, omega_s, omega_g)) == 3
        )

        baseline_covs <- lm_base |>
            dplyr::distinct(.data$pt, .data$arm, .data$study) |>
            dplyr::mutate(arm_n = as.numeric(factor(as.character(.data$arm)))) |>
            dplyr::mutate(eta_b = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(eta_s = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(eta_g = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(psi_b = exp(log(mu_b) + .data$eta_b * omega_b)) |>
            dplyr::mutate(psi_s = exp(log(mu_s[.data$arm_n]) + .data$eta_s * omega_s)) |>
            dplyr::mutate(psi_g = exp(log(mu_g[.data$arm_n]) + .data$eta_g * omega_g))

        lm_dat <- lm_base |>
            dplyr::select(!dplyr::all_of(c("study", "arm"))) |>
            dplyr::left_join(baseline_covs, by = "pt") |>
            dplyr::mutate(mu_sld = sf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
            dplyr::mutate(dsld = sf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
            dplyr::mutate(ttg = sf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g)) |>
            dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * sigma)) |>
            dplyr::mutate(log_haz_link = link_dsld * .data$dsld + link_ttg * .data$ttg)

        if (!.debug) {
            lm_dat <- lm_dat |> dplyr::select(dplyr::all_of(c("pt", "time", "sld", "log_haz_link", "study", "arm")))
        }
        return(lm_dat)
    }
}




