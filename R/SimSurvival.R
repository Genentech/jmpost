
#' `SimSurvival` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`SimSurvival`]
#' constructors.
#'
#' @param time_max (`number`)\cr the maximum time to simulate to.
#' @param time_step (`number`)\cr the time interval between evaluating the log-hazard function.
#' @param lambda_censor (`number`)\cr the censoring rate.
#' @param beta_cont (`number`)\cr the continuous covariate coefficient.
#' @param beta_cat (`numeric`)\cr the categorical covariate coefficients.
#' @param loghazard (`function`)\cr the log hazard function.
#' @param name (`character`)\cr the name of the object.
#' @param ... Not Used.
#'
#' @section Hazard Evaluation:
#'
#' Event times are simulated by sampling a cumulative hazard limit from a \eqn{U(0, 1)} distribution
#' for
#' each subject and then counting how much hazard they've been exposed to by evaluating the
#' log-hazard function at a set interval. The `time_max` argument sets the upper bound for the
#' number of time points to evaluate the log-hazard function at with subjects who have not had an
#' event being censored at `time_max`. The `time_step` argument sets the interval at which to
#' evaluate the log-hazard function. Setting smaller values for `time_step` will increase the
#' precision of the simulation at the cost of increased computation time. Likewise, setting large
#' values for `time_max` will minimize the number of censored subjects at the cost of
#' increased computation time.
#'
#' @name SimSurvival-Shared
#' @keywords internal
NULL


#' Abstract Simulation Class for Survival Data
#'
#' @inheritParams SimSurvival-Shared
#' @inheritSection SimSurvival-Shared Hazard Evaluation
#'
#' @slot time_max (`numeric`)\cr See arguments.
#' @slot time_step (`numeric`)\cr See arguments.
#' @slot lambda_censor (`numeric`)\cr See arguments.
#' @slot beta_cont (`numeric`)\cr See arguments.
#' @slot beta_cat (`numeric`)\cr See arguments.
#' @slot loghazard (`function`)\cr See arguments.
#' @slot name (`character`)\cr See arguments.
#'
#' @family SimSurvival
#' @exportClass SimSurvival
#' @name SimSurvival-class
.SimSurvival <- setClass(
    "SimSurvival",
    slots = c(
        time_max = "numeric",
        time_step = "numeric",
        lambda_censor = "numeric",
        beta_cont = "numeric",
        beta_cat = "numeric",
        loghazard = "function",
        name = "character"
    )
)

#' @rdname SimSurvival-class
#' @export
SimSurvival <- function(
    time_max = 2000,
    time_step = 1,
    lambda_censor = 1 / 3000,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    loghazard,
    name = "SimSurvival"
) {
    .SimSurvival(
        time_max = time_max,
        time_step = time_step,
        lambda_censor = lambda_censor,
        beta_cont = beta_cont,
        beta_cat = beta_cat,
        loghazard = loghazard,
        name = name
    )
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "SimSurvival",
    definition = function(object) {
        x <- sprintf("\nA %s Object\n\n", as_print_string(object))
        cat(x)
        return(object)
    }
)

#' @rdname as_print_string
as_print_string.SimSurvival <- function(object) {
    return(object@name)
}


#' Construct Time Intervals
#'
#' @param object (`SimSurvival`)\cr the survival simulation object to create evaluation points for.
#'
#' @return A `tibble` with `lower`, `upper`, `time`, `eval` and `width`.
#' @keywords internal
hazardWindows.SimSurvival <- function(object) {
    times <- seq(0, object@time_max, object@time_step)
    bound_lower <- times[-length(times)]
    bound_upper <- times[-1]
    bound_width <- bound_upper - bound_lower
    mid_point <- bound_upper - (bound_width / 2)
    tibble::tibble(
        lower = bound_lower,
        upper = bound_upper,
        midpoint = mid_point,
        width = bound_width
    )
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimSurvival <- function(object, subjects_df) {
    subjects_df |>
        dplyr::mutate(cov_cont = stats::rnorm(dplyr::n())) |>
        dplyr::mutate(cov_cat = factor(
            sample(names(object@beta_cat), replace = TRUE, size = dplyr::n()),
            levels = names(object@beta_cat)
        )) |>
        dplyr::mutate(log_haz_cov = .data$cov_cont * object@beta_cont + object@beta_cat[.data$cov_cat]) |>
        dplyr::mutate(survival = stats::runif(dplyr::n())) |>
        dplyr::mutate(chazard_limit = -log(.data$survival)) |>
        dplyr::mutate(time_cen = stats::rexp(dplyr::n(), object@lambda_censor))
}


#' @rdname sampleObservations
#' @export
sampleObservations.SimSurvival <- function(object, times_df) {

    assert_that(
        all(times_df$time >= 0),
        msg = "All time points must be greater than or equal to 0"
    )

    os_dat_chaz <- times_df |>
        dplyr::mutate(log_bl_haz = object@loghazard(.data$midpoint)) |>
        # Fix to avoid issue with log(0) = NaN values
        dplyr::mutate(log_bl_haz = dplyr::if_else(.data$midpoint == 0, -999, .data$log_bl_haz)) |>
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

    if (!(nrow(os_had_censor) == 0)) {
        message(sprintf("INFO: %i patients did not die before max(times)", nrow(os_had_censor)))
    }

    os_dat_complete <- os_had_event |>
        dplyr::bind_rows(os_had_censor) |>
        dplyr::mutate(real_time = .data$time) |>
        dplyr::mutate(event = dplyr::if_else(.data$real_time <= .data$time_cen, .data$event, 0)) |>
        dplyr::mutate(time = dplyr::if_else(.data$real_time <= .data$time_cen, .data$real_time, .data$time_cen)) |>
        dplyr::arrange(.data$pt)

    os_dat_complete[, c("pt", "study", "arm", "time", "event", "cov_cont", "cov_cat")]
}


#' Simulate Survival Data from a Weibull Proportional Hazard Model
#'
#' @param lambda (`number`)\cr the scale parameter.
#' @param gamma (`number`)\cr the shape parameter.
#'
#' @inheritParams SimSurvival-Shared
#' @inheritSection SimSurvival-Shared Hazard Evaluation
#'
#' @family SimSurvival
#'
#' @export
SimSurvivalWeibullPH <- function(
    lambda,
    gamma,
    time_max = 2000,
    time_step = 1,
    lambda_censor = 1 / 3000,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2)
) {
    SimSurvival(
        time_max = time_max,
        time_step = time_step,
        lambda_censor = lambda_censor,
        beta_cont = beta_cont,
        beta_cat = beta_cat,
        loghazard = function(time) {
            log(lambda) + log(gamma) + (gamma - 1) * log(time)
        },
        name = "SimSurvivalWeibullPH"
    )
}

#' Simulate Survival Data from a Log-Logistic Proportional Hazard Model
#'
#' @param a (`number`)\cr the scale parameter.
#' @param b (`number`)\cr the shape parameter.
#'
#' @inheritParams SimSurvival-Shared
#' @inheritSection SimSurvival-Shared Hazard Evaluation
#'
#' @family SimSurvival
#' @export
SimSurvivalLogLogistic <- function(
    a,
    b,
    time_max = 2000,
    time_step = 1,
    lambda_censor = 1 / 3000,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2)
) {
    SimSurvival(
        time_max = time_max,
        time_step = time_step,
        lambda_censor = lambda_censor,
        beta_cont = beta_cont,
        beta_cat = beta_cat,
        loghazard = function(time) {
            c1 <- - log(a) + log(b) + (b - 1) * (- log(a) + log(time))
            c2 <- log(1 + (time / a)^b)
            return(c1 - c2)
        },
        name = "SimSurvivalLogLogistic"
    )
}



#' Simulate Survival Data from a Exponential Proportional Hazard Model
#'
#' @param lambda (`number`)\cr the rate parameter.
#'
#' @inheritParams SimSurvival-Shared
#' @inheritSection SimSurvival-Shared Hazard Evaluation
#'
#' @family SimSurvival
#'
#' @export
SimSurvivalExponential <- function(
    lambda,
    time_max = 2000,
    time_step = 1,
    lambda_censor = 1 / 3000,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2)
) {
    SimSurvival(
        time_max = time_max,
        time_step = time_step,
        lambda_censor = lambda_censor,
        beta_cont = beta_cont,
        beta_cat = beta_cat,
        loghazard = function(time) {
            log(lambda)
        },
        name = "SimSurvivalExponential"
    )
}
