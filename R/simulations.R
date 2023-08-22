
#' Construct Time Intervals
#'
#' @param x (`numeric`)\cr grid of time points.
#'
#' @return A `tibble` with `lower`, `upper`, `time`, `eval` and `width`.
#' @keywords internal
get_timepoints <- function(x) {
    assert_that(length(x) == length(unique(x)))
    x_ord <- x[order(x)]
    x_no_neg <- x_ord[x_ord >= 0]

    bound_lower <- c(0, x_no_neg[-length(x_no_neg)])
    bound_upper <- x_no_neg
    bound_width <- bound_upper - bound_lower
    eval_points <- bound_upper - bound_width / 2
    tibble::tibble(
        lower = bound_lower,
        upper = bound_upper,
        eval = eval_points,
        time = bound_upper,
        width = bound_width
    )
}


#' Simulating Joint Longitudinal and Time-to-Event Data
#'
#' @param n_arm (`numeric`)\cr numbers of patients per treatment arm.
#' @param times (`numeric`)\cr time grid, e.g. specifying the days after randomization.
#' @param lambda_cen (`number`)\cr rate of the exponential censoring distribution.
#' @param beta_cont (`number`)\cr coefficient for the continuous covariate.
#' @param beta_cat (`numeric`)\cr coefficients for the categorical covariate levels.
#' @param lm_fun (`function`)\cr function of `lm_base` generating the longitudinal model outcomes.
#' @param os_fun (`function`)\cr function of `lm_base` generating the survival model outcomes.
#' @param .silent (`flag`)\cr whether to suppress info messages
#' @param .debug (`flag`)\cr whether to enter debug mode such that the function
#'   would only return a subset of columns.
#'
#' @returns List with simulated `lm` (longitudinal) and `os` (survival) data sets.
#' @export
simulate_joint_data <- function(
    n_arm = c(50, 80),   # Number of arms and number of subjects per arm
    times = 0:2000,
    lambda_cen = 1 / 3,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    lm_fun,
    os_fun,
    .silent = FALSE,
    .debug = FALSE
) {
    n <- sum(n_arm)
    u_pts <- sprintf("pt_%05i", seq_len(n))
    bounds <- get_timepoints(times)

    ARMS <- paste0("Group-", seq_along(n_arm))

    os_baseline <- dplyr::tibble(pt = u_pts) |>
        dplyr::mutate(cov_cont = stats::rnorm(n)) |>
        dplyr::mutate(cov_cat = factor(
            sample(c("A", "B", "C"), replace = TRUE, size = n),
            levels = c("A", "B", "C")
        )) |>
        dplyr::mutate(log_haz_cov = .data$cov_cont * beta_cont + beta_cat[.data$cov_cat]) |>
        dplyr::mutate(survival = stats::runif(n)) |>
        dplyr::mutate(chazard_limit = -log(.data$survival)) |>
        dplyr::mutate(time_cen = stats::rexp(n, lambda_cen)) |>
        dplyr::mutate(study = factor("Study-1")) |>
        dplyr::mutate(arm = factor(rep(ARMS, times = n_arm), levels = ARMS)) |>
        dplyr::select(!dplyr::all_of("survival"))

    time_dat <- expand.grid(
        time = as.double(bounds$time),
        pt = u_pts,
        stringsAsFactors = FALSE
    ) |>
        tibble::as_tibble() |>
        dplyr::mutate(width = rep(bounds$width, times = n)) |>
        dplyr::mutate(evalp = rep(bounds$eval, times = n)) |>
        dplyr::select("pt", "time", "evalp", "width")

    time_dat_baseline <- time_dat |>
        dplyr::left_join(os_baseline, by = "pt")

    lm_dat <- time_dat_baseline |>
        dplyr::select(dplyr::all_of(c("pt", "time", "evalp", "arm", "study"))) |>
        lm_fun()

    assert_that(
        all(lm_dat$pt == time_dat_baseline$pt),
        all(lm_dat$time == time_dat_baseline$time),
        msg = "The longitudinal dataset must be sorted by pt, time"
    )

    os_dat_chaz <- time_dat_baseline |>
        dplyr::mutate(log_haz_link = lm_dat$log_haz_link) |> # only works if lm_dat is sorted pt, time
        dplyr::mutate(log_bl_haz = os_fun(.data$evalp)) |>
        # Fix to avoid issue with log(0) = NaN values
        dplyr::mutate(log_bl_haz = dplyr::if_else(.data$evalp == 0, -999, .data$log_bl_haz)) |>
        dplyr::mutate(hazard_instant = exp(.data$log_bl_haz + .data$log_haz_cov + .data$log_haz_link)) |>
        dplyr::mutate(hazard_interval = .data$hazard_instant * .data$width) |>
        dplyr::group_by(.data$pt) |>
        dplyr::mutate(chazard = cumsum(.data$hazard_interval)) |>
        dplyr::ungroup()

    os_had_event <- os_dat_chaz |>
        dplyr::filter(.data$chazard >= .data$chazard_limit) |>
        dplyr::group_by(.data$pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = 1)

    os_had_censor <- os_dat_chaz |>
        dplyr::filter(!.data$pt %in% os_had_event$pt) |>
        dplyr::group_by(.data$pt) |>
        dplyr::slice(dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = 0)

    if (!(nrow(os_had_censor) == 0) && !.silent) {
        message(sprintf("INFO: %i patients did not die before max(times)", nrow(os_had_censor)))
    }

    os_dat_complete <- os_had_event |>
        dplyr::bind_rows(os_had_censor) |>
        dplyr::mutate(real_time = .data$time) |>
        dplyr::mutate(event = dplyr::if_else(.data$real_time <= .data$time_cen, .data$event, 0)) |>
        dplyr::mutate(time = dplyr::if_else(.data$real_time <= .data$time_cen, .data$real_time, .data$time_cen)) |>
        dplyr::arrange(.data$pt)

    os_time <- rep(os_dat_complete$time, each = length(bounds$time))

    assert_that(
        length(os_time) == nrow(lm_dat),
        all(lm_dat$pt == rep(os_dat_complete$pt, each = length(bounds$time)))
    )

    lm_dat2 <- lm_dat |>
        dplyr::mutate(observed = (.data$time <= os_time))

    assert_that(
        length(unique(os_dat_complete$pt)) == length(os_dat_complete$pt),
        length(os_dat_complete$pt) == n,
        all(os_dat_complete$time >= 0),
        all(os_dat_complete$event %in% c(0, 1)),
        nrow(lm_dat2) == n * length(times)
    )

    if (!.debug) {
        os_dat_complete <- os_dat_complete |>
            dplyr::select(dplyr::all_of(c("pt", "time", "event", "cov_cont", "cov_cat", "study", "arm")))

        lm_dat2 <- lm_dat2 |>
            dplyr::select(dplyr::all_of(c("pt", "time", "sld", "study", "arm", "observed")))
    }

    return(list(os = os_dat_complete, lm = lm_dat2))
}
