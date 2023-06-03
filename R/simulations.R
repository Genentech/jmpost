#' Construct a Variance-Covariance Matrix from Standard Deviations and Correlations
#'
#' This function creates a variance-covariance matrix based on the input standard deviations
#' and correlations. Note that the values may be altered using [Matrix::nearPD()]
#' in order to ensure that it is a valid positive semi-definite matrix.
#'
#' @param sd A numeric vector containing the standard deviations of the variables.
#' @param cor A numeric vector containing the pairwise correlations between the variables.
#' The vector should be in the order of the upper triangular part of the matrix, excluding
#' the diagonal elements. e.g. for a 3x3 matrix c(0.2, 0.3, 0.4) would be the values for
#' x_12, x_13 and x_23 of the correlation matrix respectively
#'
#'
#' @returns A symmetric matrix representing the variance-covariance matrix calculated from
#' the input standard deviations and correlations. The matrix is guaranteed to be positive
#' semi-definite.
#'
#' @examples
#' sd <- c(1, 2, 3)
#' cor <- c(0.5, 0.6, 0.7)
#' as_vcov(sd, cor)
#'
#' @export
as_vcov <- function(sd, cor) {
    x <- diag(rep(1, length(sd)))
    x[upper.tri(x)] <- cor
    x <- t(x)
    x[upper.tri(x)] <- cor
    res <- diag(sd) %*% x %*% diag(sd)
    res <- as.matrix(Matrix::nearPD(res)$mat)
    assert_that(isSymmetric(res))
    dimnames(res) <- NULL
    return(res)
}

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
gsf_dsld <- function(time, b , s, g, phi) {
    t1 <- (1 - phi) * g * exp(g * time)
    t2 <- phi * s * exp(-s * time)
    return(b * (t1 - t2))
}

#' Construct a Simulation Function for Longitudinal Data from GSF Model
#'
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param mu_s (`numeric`)\cr the mean shrinkage rates for the two treatment arms.
#' @param mu_g (`numeric`)\cr the mean growth rates for the two treatment arms.
#' @param mu_phi (`numeric`)\cr the mean shrinkage proportions for the two treatment arms.
#' @param mu_b (`numeric`)\cr the mean baseline values for the two treatment arms.
#' @param omega_b (`number`)\cr the baseline value standard deviation.
#' @param omega_s (`number`)\cr the shrinkage rate standard deviation.
#' @param omega_g (`number`)\cr the growth rate standard deviation.
#' @param omega_phi (`number`)\cr the shrinkage proportion standard deviation.
#' @param link_dsld (`number`)\cr the link coefficient for the derivative contribution.
#' @param link_ttg (`number`)\cr the link coefficient for the time-to-growth contribution.
#' @param .debug (`flag`)\cr whether to enter debug mode such that the function
#'   would only return a subset of columns.
#'
#' @returns A function with argument `lm_base` that can be used to simulate
#'   longitudinal data from the corresponding GSF model.
#'
#' @export
sim_lm_gsf <- function(
    sigma = 0.01,
    mu_s = c(3, 4),
    mu_g = c(0.2, 0.3),
    mu_phi = c(0.1, 0.2),
    mu_b = c(50, 60),
    omega_b = 0.135,
    omega_s = 0.15,
    omega_g = 0.225,
    omega_phi = 0.75,
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
            length(mu_s) == length(mu_phi),
            length(c(omega_b, omega_s, omega_g, omega_phi)) == 4
        )

        baseline_covs <- lm_base |>
            dplyr::distinct(.data$pt, .data$arm, .data$study) |>
            dplyr::mutate(arm_n = as.numeric(factor(as.character(.data$arm)))) |>
            dplyr::mutate(eta_b = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(eta_s = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(eta_g = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(eta_phi = stats::rnorm(dplyr::n(), 0, 1)) |>
            dplyr::mutate(psi_b = exp(log(mu_b) + .data$eta_b * omega_b)) |>
            dplyr::mutate(psi_s = exp(log(mu_s[.data$arm_n]) + .data$eta_s * omega_s)) |>
            dplyr::mutate(psi_g = exp(log(mu_g[.data$arm_n]) + .data$eta_g * omega_g)) |>
            dplyr::mutate(psi_phi = stats::plogis(stats::qlogis(mu_phi[.data$arm_n]) + .data$eta_phi * omega_phi))

        lm_dat <- lm_base |>
            dplyr::select(!dplyr::all_of(c("study", "arm"))) |>
            dplyr::left_join(baseline_covs, by = "pt") |>
            dplyr::mutate(mu_sld = gsf_sld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(dsld = gsf_dsld(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(ttg = gsf_ttg(.data$time, .data$psi_b, .data$psi_s, .data$psi_g, .data$psi_phi)) |>
            dplyr::mutate(sld = stats::rnorm(dplyr::n(), .data$mu_sld, .data$mu_sld * sigma)) |>
            dplyr::mutate(log_haz_link = link_dsld * .data$dsld + link_ttg * .data$ttg)

        if (!.debug) {
            lm_dat <- lm_dat |> dplyr::select(dplyr::all_of(c("pt", "time", "sld", "log_haz_link", "study", "arm")))
        }
        return(lm_dat)
    }
}


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

        if ( ! .debug) {
            lm_dat <- lm_dat |> dplyr::select(dplyr::all_of(c("pt", "time", "sld", "log_haz_link", "study", "arm")))
        }
        return(lm_dat)
    }
}


#' Construct a Log Hazard Function for the Weibull Model
#'
#' @param lambda (`number`)\cr the scale parameter.
#' @param gamma (`number`)\cr the shape parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_weibull <- function(lambda, gamma) {
    function(time) {
        log(lambda) + log(gamma) + (gamma - 1) * log(time)
    }
}

#' Construct a Log Hazard Function for the Exponential Model
#'
#' @param lambda (`number`)\cr the rate parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_exponential <- function(lambda) {
    function(time) {
        log(lambda)
    }
}

#' Construct a Log Hazard Function for the Log-Logistic Model
#'
#' @param lambda (`number`)\cr the inverse median parameter.
#' @param p (`number`)\cr the shape parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_loglogistic <- function(lambda, p){
    function(time) {
        c1 <- log(lambda) + log(p) + (p - 1) * (log(lambda) + log(time))
        c2 <- log(1 + (lambda * time)^p)
        return(c1 - c2)
    }
}

#' Construct Time Intervals
#'
#' @param x (`numeric`)\cr grid of time points.
#'
#' @return A `tibble` with `lower`, `upper`, `time` and `width`.
#' @keywords internal
get_timepoints <- function(x) {
    assert_that(length(x) == length(unique(x)))
    x_ord <- x[order(x)]
    x_no_neg <- x_ord[x_ord > 0]

    bound_lower <- c(0, x_no_neg[-length(x_no_neg)])
    bound_upper <- x_no_neg
    bound_width <- bound_upper - bound_lower
    eval_points <- bound_upper
    tibble::tibble(
        lower = bound_lower,
        upper = bound_upper,
        time = eval_points,
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
#'
#' @returns List with simulated `lm` (longitudinal) and `os` (survival) data sets.
#' @export
simulate_joint_data <- function(
    n_arm = c(50, 80),   # Number of arms and number of subjects per arm
    times = 1:2000,
    lambda_cen = 1 / 3,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    lm_fun,
    os_fun
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
        dplyr::mutate(arm = factor(rep(ARMS, times = n_arm), levels = ARMS))

    time_dat <- expand.grid(
        time = as.double(bounds$time),
        pt = u_pts,
        stringsAsFactors = FALSE
    ) |>
        tibble::as_tibble() |>
        dplyr::mutate(width = rep(bounds$width, times = n))

    time_dat_baseline <- time_dat |>
        dplyr::left_join(os_baseline, by = "pt")

    lm_dat <- time_dat_baseline |>
        dplyr::select(dplyr::all_of(c("pt", "time", "arm", "study"))) |>
        lm_fun()

    assert_that(
        all(lm_dat$pt == time_dat_baseline$pt),
        all(lm_dat$time == time_dat_baseline$time),
        msg = "The longitudinal dataset must be sorted by pt, time"
    )

    os_dat_chaz <- time_dat_baseline |>
        dplyr::mutate(log_haz_link = lm_dat$log_haz_link) |> # only works if lm_dat is sorted pt, time
        dplyr::mutate(log_bl_haz = os_fun(.data$time)) |>
        dplyr::mutate(hazard_instant = exp(.data$log_bl_haz + .data$log_haz_cov + .data$log_haz_link)) |>
        dplyr::mutate(hazard_interval = .data$hazard_instant * .data$width) |>
        dplyr::group_by(.data$pt) |>
        dplyr::mutate(chazard = cumsum(.data$hazard_interval)) |>
        dplyr::ungroup()

    os_dat <- os_dat_chaz|>
        dplyr::filter(.data$chazard_limit <= .data$chazard) |>
        dplyr::select(dplyr::all_of(c("pt", "time", "cov_cont", "cov_cat", "time_cen", "study", "arm"))) |>
        dplyr::group_by(.data$pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = dplyr::if_else(.data$time <= .data$time_cen, 1, 0)) |>
        dplyr::mutate(time = dplyr::if_else(.data$event == 1, .data$time, .data$time_cen)) |>
        dplyr::select(dplyr::all_of(c("pt", "time", "cov_cont", "cov_cat", "event", "study", "arm")))

    os_dat_censor <- os_dat_chaz |>
        dplyr::group_by(.data$pt) |>
        dplyr::slice(1) |>
        dplyr::mutate(time = max(times)) |>
        dplyr::mutate(event = 0) |>
        dplyr::select(dplyr::all_of(c("pt", "time", "cov_cont", "cov_cat", "event", "study", "arm"))) |>
        dplyr::filter(!.data$pt %in% os_dat$pt)

    if (!nrow(os_dat_censor)== 0 ) {
        message(sprintf("%i patients did not die before max(times)", nrow(os_dat_censor)))
    }

    os_dat_complete <- os_dat |>
        dplyr::bind_rows(os_dat_censor) |>
        dplyr::arrange(.data$pt)

    os_time <- rep(os_dat_complete$time, each = length(bounds$time))

    assert_that(
        length(os_time) == nrow(lm_dat),
        all(lm_dat$pt == rep(os_dat_complete$pt, each = length(bounds$time)))
    )

    lm_dat2 <- lm_dat |>
        dplyr::mutate(os_time = os_time) |>
        dplyr::mutate(observed = (.data$time <= .data$os_time)) |>
        dplyr::select(!dplyr::all_of(c("os_time", "log_haz_link")))

    assert_that(
        length(unique(os_dat_complete$pt)) == n,
        n == nrow(os_dat_complete),
        all(os_dat_complete$time >= 0),
        all(os_dat_complete$event %in% c(0, 1)),
        nrow(lm_dat2) == n * length(bounds$time),
        all(!is.na(lm_dat2$observed))
    )
    return(list(os = os_dat_complete, lm = lm_dat2))
}
