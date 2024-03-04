
#' Construct a Simulation Function for Longitudinal Data from Random Slope Model
#'
#' @param intercept (`number`)\cr the mean baseline value for each study.
#' @param slope_mu (`numeric`)\cr the population slope for each treatment arm.
#' @param slope_sigma (`number`)\cr the random slope standard deviation.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param link_dsld (`number`)\cr the link coefficient for the DSLD contribution.
#' @param link_identity (`number`)\cr the link coefficient for the identity contribution.
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
    link_dsld = 0,
    link_identity = 0
) {
    function(lm_base) {

        assert_that(
            is.factor(lm_base$study),
            is.factor(lm_base$arm)
        )

        assert_that(
            length(slope_mu) == length(unique(lm_base$arm)),
            msg = "`length(slope_mu)` should be equal to the number of unique studies"
        )

        assert_that(
            length(intercept) == length(unique(lm_base$study)),
            msg = "`length(intercept)` should be equal to the number of unique studies"
        )

        rs_baseline <- lm_base |>
            dplyr::distinct(.data$pt, .data$arm, .data$study) |>
            dplyr::mutate(intercept = intercept[as.numeric(.data$study)]) |>
            dplyr::mutate(slope_ind = stats::rnorm(
                n = dplyr::n(),
                mean = slope_mu[as.numeric(.data$arm)],
                sd = slope_sigma
            )) |>
            dplyr::select(!dplyr::any_of(c("arm", "study")))

        lm_dat <- lm_base |>
            dplyr::mutate(err = stats::rnorm(dplyr::n(), 0, sigma)) |>
            dplyr::left_join(rs_baseline, by = "pt") |>
            dplyr::mutate(sld = intercept + .data$slope_ind * .data$time + .data$err) |>
            dplyr::mutate(log_haz_link = .data$slope_ind * link_dsld + .data$sld * link_identity)

        return(lm_dat)
    }
}
