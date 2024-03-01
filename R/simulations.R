
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

#' Define Simulation Group
#'
#' Specifies a simulation group to be used by [`simulate_joint_data()`].
#'
#' @param n (`numeric`)\cr number of subjects in the group.
#' @param arm (`character`)\cr treatment arm.
#' @param study (`character`)\cr study name.
#'
#' @slot n (`numeric`)\cr See arguments.
#' @slot arm (`character`)\cr See arguments.
#' @slot study (`character`)\cr See arguments.
#'
#' @examples
#' SimGroup(n = 50, arm = "Arm-A", study = "Study-1")
#' @name SimGroup-class
#' @exportClass SimGroup
.SimGroup <- setClass(
    "SimGroup",
    slots = c(
        n = "numeric",
        study = "character",
        arm = "character"
    )
)

#' @export
#' @rdname SimGroup-class
SimGroup <- function(n, arm, study) {
    .SimGroup(
        n = n,
        arm = arm,
        study = study
    )
}


setValidity(
    "SimGroup",
    function(object) {
        if (length(object@n) != 1) {
            return("`n` must be a length 1 integer")
        }
        if (length(object@arm) != 1) {
            return("`arm` must be a length 1 string")
        }
        if (length(object@study) != 1) {
            return("`study` must be a length 1 string")
        }
        if (any(object@n < 1) | any(object@n %% 1 != 0)) {
            return("`n` must be positive integer")
        }
        return(TRUE)
    }
)

#' Simulating Joint Longitudinal and Time-to-Event Data
#'
#' @param design (`list`)\cr a list of [`SimGroup`] objects. See details.
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
#' @details
#'
#' The `design` argument is used to specify how many distinct groups should be simulated
#' including key information such as the number of subjects within the group as well as
#' which treatment arm and study the group belongs to. The `design` argument should be a
#' list of [`SimGroup`] objects e.g.
#' ```
#' design = list(
#'     SimGroup(n = 50, study = "Study-1", arm = "Arm-A"),
#'     SimGroup(n = 50, study = "Study-1", arm = "Arm-B")
#' )
#' ```
#'
#' @returns List with simulated `lm` (longitudinal) and `os` (survival) data sets.
#' @export
simulate_joint_data <- function(
    design = list(
        SimGroup(n = 50, study = "Study-1", arm = "Arm-A"),
        SimGroup(n = 50, study = "Study-1", arm = "Arm-B")
    ),
    times = 0:2000,
    lambda_cen = 1 / 3,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    lm_fun,
    os_fun,
    .silent = FALSE,
    .debug = FALSE
) {

    assert(
        all(vapply(design, \(x) is(x, "SimGroup"), logical(1))),
        msg = "All elements of `design` must be of class `SimGroup`"
    )

    n_group <- vapply(design, function(x) x@n, numeric(1))
    arms <- vapply(design, function(x) x@arm, character(1))
    studies <- vapply(design, function(x) x@study, character(1))
    n <- sum(n_group)
    u_pts <- sprintf("pt_%05i", seq_len(n))
    bounds <- get_timepoints(times)

    os_baseline <- dplyr::tibble(pt = u_pts) |>
        dplyr::mutate(cov_cont = stats::rnorm(n)) |>
        dplyr::mutate(cov_cat = factor(
            sample(names(beta_cat), replace = TRUE, size = n),
            levels = names(beta_cat)
        )) |>
        dplyr::mutate(log_haz_cov = .data$cov_cont * beta_cont + beta_cat[.data$cov_cat]) |>
        dplyr::mutate(survival = stats::runif(n)) |>
        dplyr::mutate(chazard_limit = -log(.data$survival)) |>
        dplyr::mutate(time_cen = stats::rexp(n, lambda_cen)) |>
        dplyr::mutate(arm = factor(rep(arms, times = n_group), levels = unique(arms))) |>
        dplyr::mutate(study = factor(rep(studies, times = n_group), levels = unique(studies))) |>
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
        # Reset Inf values to large number to avoid NaN issues downstream
        # This is suitable as Hazard limits tend to be in the range of -10 to 10 so large numbers
        # are essentially equivalent to infinity for simulation purposes
        dplyr::mutate(hazard_instant = dplyr::if_else(.data$hazard_instant == Inf, 999, .data$hazard_instant)) |>
        dplyr::mutate(hazard_instant = dplyr::if_else(.data$hazard_instant == -Inf, -999, .data$hazard_instant)) |>
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
