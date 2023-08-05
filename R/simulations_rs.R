
#' Construct a Simulation Function for Longitudinal Data from Random Slope Model
#'
#' @param intercept (`number`)\cr the mean baseline value.
#' @param slope_mu (`numeric`)\cr the population slope for the two treatment arms.
#' @param slope_sigma (`number`)\cr the random slope standard deviation.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param phi (`number`)\cr the link coefficient for the random slope contribution.
#' @param .debug (`flag`)\cr whether to enter debug mode such that the function
#'   would only return a subset of columns.
#'
#' @returns A function with argument `lm_base` that can be used to simulate
#'   longitudinal data from the corresponding random slope model.
#'
#' @export
sim_lm_random_slope <- function(
    intercept = 50,
    slope_mu = c(0.01, 0.03),
    slope_sigma = 0.5,
    sigma = 2,
    phi = 0.1,
    .debug = FALSE
) {
    function(lm_base) {

        assert_that(
            length(slope_mu) == 1 | length(slope_mu) == length(unique(lm_base$arm)),
            msg = "slope_mu should either be length 1 or equal to the length of n_arm"
        )

        if (length(slope_mu) == 1) {
            slope_mu <- rep(slope_mu, length(unique(lm_base$arm)))
        }

        rs_baseline <- lm_base |>
            dplyr::distinct(.data$pt, .data$arm) |>
            dplyr::mutate(slope_ind = stats::rnorm(
                dplyr::n(), slope_mu[as.numeric(factor(as.character(.data$arm)))], sd = slope_sigma
            )) |>
            dplyr::select(!dplyr::any_of("arm"))

        lm_dat <- lm_base |>
            dplyr::mutate(err = stats::rnorm(dplyr::n(), 0, sigma)) |>
            dplyr::left_join(rs_baseline, by = "pt") |>
            dplyr::mutate(sld = intercept + .data$slope_ind * .data$time + .data$err) |>
            dplyr::mutate(log_haz_link = .data$slope_ind * phi)

        if (!.debug) {
            lm_dat <- lm_dat |> dplyr::select(dplyr::all_of(c("pt", "time", "sld", "log_haz_link", "study", "arm")))
        }
        return(lm_dat)
    }
}
