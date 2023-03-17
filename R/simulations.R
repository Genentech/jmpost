


#' @export
as_vcov <- function(sd, cor) {
    x <- diag(rep(1, length(sd)))
    x[upper.tri(x)] <- cor
    x <- t(x)
    x[upper.tri(x)] <- cor
    res <- diag(sd) %*% x %*% diag(sd)
    res <- as.matrix(Matrix::nearPD(res)$mat)
    assertthat::assert_that(isSymmetric(res))
    dimnames(res) <- NULL
    return(res)
}


#' @export
sld <- function(time, b, s, g, phi) {
    b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
}


ttg <- function(time, b, s, g, phi) {
    t1 <- (log(s * phi / (g * (1 - phi))) / (g + s))
    t1[t1 <= 0] <- 0
    return(t1)
}


dsld <- function(time, b , s, g, phi) {
    t1 <- (1 - phi) * g * exp(g * time)
    t2 <- phi * s * exp(-s * time)
    return(b * (t1 - t2))
}



#' @export
sim_lm_gsf <- function(
    sigma = 0.01,
    mu_s = c(3, 4),
    mu_g = c(0.2, 0.3),
    mu_phi = c(0.1, 0.2),
    mu_b = c(50, 60),
    eta_b_sigma = 5,
    eta_s_sigma = 2,
    eta_g_sigma = 1,
    eta_phi_sigma = 5,
    omega_b = 0.135,
    omega_s = 0.15,
    omega_g = 0.225,
    omega_phi = 0.75,
    link_dsld = 0,
    link_ttg = 0,
    .debug = FALSE
) {
    function(lm_base) {
        
        assertthat::assert_that(
            length(unique(lm_base$study)) == 1,
            length(mu_b) == 1,
            length(sigma) == 1,
            length(mu_s) == length(unique(lm_base$arm)),
            length(mu_s) == length(mu_g),
            length(mu_s) == length(mu_phi),
            length(c(eta_b_sigma, eta_g_sigma, eta_phi_sigma, eta_s_sigma)) == 4,
            length(c(omega_b, omega_s, omega_g, omega_phi)) == 4
        )

        baseline_covs <- lm_base |>
            dplyr::distinct(pt, arm, study) |>
            dplyr::mutate(arm_n = as.numeric(factor(as.character(arm)))) |>
            dplyr::mutate(eta_b = rnorm(dplyr::n(), 0, eta_b_sigma)) |>
            dplyr::mutate(eta_s = rnorm(dplyr::n(), 0, eta_s_sigma)) |>
            dplyr::mutate(eta_g = rnorm(dplyr::n(), 0, eta_g_sigma)) |>
            dplyr::mutate(eta_phi = rnorm(dplyr::n(), 0, eta_phi_sigma)) |>
            dplyr::mutate(psi_b = exp(log(mu_b) + eta_b * omega_b)) |>
            dplyr::mutate(psi_s = exp(log(mu_s[arm_n]) + eta_s * omega_s)) |>
            dplyr::mutate(psi_g = exp(log(mu_g[arm_n]) + eta_g * omega_g)) |>
            dplyr::mutate(psi_phi = plogis(qlogis(mu_phi[arm_n]) + eta_phi * omega_phi))

        lm_dat <- lm_base |>
            dplyr::select(-study, -arm) |>
            dplyr::left_join(baseline_covs, by = "pt") |>
            dplyr::mutate(mu_sld = sld(time, psi_b, psi_s, psi_g, psi_phi)) |>
            dplyr::mutate(dsld = dsld(time, psi_b, psi_s, psi_g, psi_phi)) |>
            dplyr::mutate(ttg = ttg(time, psi_b, psi_s, psi_g, psi_phi)) |>
            dplyr::mutate(sld = rnorm(dplyr::n(), mu_sld, mu_sld * sigma)) |>
            dplyr::mutate(log_haz_link = link_dsld * dsld + link_ttg * ttg)

        if (!.debug) {
            lm_dat <- lm_dat |> dplyr::select(pt, time, sld, log_haz_link, study, arm)
        }
        return(lm_dat)
    }
}


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
        
        assertthat::assert_that(
            length(slope_mu) == 1 | length(slope_mu) == length(unique(lm_base$arm)),
            msg = "slope_mu should either be length 1 or equal to the length of n_arm"
        )
        
        if (length(slope_mu) == 1) {
            slope_mu <- rep(slope_mu, length(unique(lb_base$arm)))
        }
        
        rs_baseline <- lm_base |>
            dplyr::distinct(pt, arm) |>
            dplyr::mutate(slope_ind = rnorm(dplyr::n(), slope_mu[as.numeric(arm)], sd = slope_sigma)) |>
            dplyr::select(-arm)

        lm_dat <- lm_base |>
            dplyr::mutate(err = rnorm(dplyr::n(), 0, sigma)) |>
            dplyr::left_join(rs_baseline, by = "pt") |>
            dplyr::mutate(sld = intercept + slope_ind * time + err) |>
            dplyr::mutate(log_haz_link = slope_ind * phi)
        
        if ( ! .debug) {
            lm_dat <- lm_dat |> dplyr::select(pt, time, sld, log_haz_link, study, arm)
        }
        return(lm_dat)
    }
}


#' @export
sim_os_weibull <- function(lambda, gamma) {
    function(time) {
        log(lambda) + log(gamma) + (gamma - 1) * log(time)
    }
}

#' @export
sim_os_exponential <- function(lambda) {
    function(time) {
        log(lambda)
    }
}


#' @export
sim_os_loglogistic <- function(lambda, p){
    function(time) {
        c1 <- lambda * p * (lambda * time)^(p - 1)
        c2 <- 1 + (lambda * time)^p
        log(c1 / c2)
    }
}

get_timepoints <- function(x) {
    assertthat::assert_that(length(x) == length(unique(x)))
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
        dplyr::mutate(cov_cont = rnorm(n)) |> 
        dplyr::mutate(cov_cat = factor(
            sample(c("A", "B", "C"), replace = TRUE, size = n),
            levels = c("A", "B", "C")
        )) |> 
        dplyr::mutate(log_haz_cov = cov_cont * beta_cont + beta_cat[cov_cat]) |>
        dplyr::mutate(survival = runif(n)) |>
        dplyr::mutate(chazard_limit = -log(survival)) |>
        dplyr::mutate(time_cen = rexp(n, lambda_cen)) |>
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
        dplyr::select(pt, time, arm, study) |>
        lm_fun()
    
    assert_that(
        all(lm_dat$pt == time_dat_baseline$pt),
        all(lm_dat$time == time_dat_baseline$time),
        msg = "The longitudinal dataset must be sorted by pt, time"
    )

    os_dat_chaz <- time_dat_baseline |>
        dplyr::mutate(log_haz_link = lm_dat$log_haz_link) |> # only works if lm_dat is sorted pt, time
        dplyr::mutate(log_bl_haz = os_fun(time)) |>
        dplyr::mutate(hazard_instant = exp(log_bl_haz + log_haz_cov + log_haz_link)) |>
        dplyr::mutate(hazard_interval = hazard_instant * width) |>
        dplyr::group_by(pt) |>
        dplyr::mutate(chazard = cumsum(hazard_interval)) |>
        dplyr::ungroup()
    
    os_dat <- os_dat_chaz|>
        dplyr::filter(chazard_limit <= chazard) |>
        dplyr::select(pt, time, cov_cont, cov_cat, time_cen, study, arm) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = dplyr::if_else(time <= time_cen, 1, 0)) |>
        dplyr::mutate(time = dplyr::if_else(event == 1, time, time_cen)) |>
        dplyr::select(pt, time, cov_cont, cov_cat, event, study, arm)

    os_dat_censor <- os_dat_chaz |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::mutate(time = max(times)) |>
        dplyr::mutate(event = 0) |>
        dplyr::select(pt, time, cov_cont, cov_cat, event, study, arm) |>
        dplyr::filter(!pt %in% os_dat$pt)
    
    if (!nrow(os_dat_censor)== 0 ) {
        message(sprintf("%i patients did not die before max(times)", nrow(os_dat_censor)))
    }

    os_dat_complete <- os_dat |>
        dplyr::bind_rows(os_dat_censor) |>
        dplyr::arrange(pt)

    os_time <- rep(os_dat_complete$time, each = length(bounds$time))
    
    assert_that(
        length(os_time) == nrow(lm_dat),
        all(lm_dat$pt == rep(os_dat_complete$pt, each = length(bounds$time)))
    )

    lm_dat2 <- lm_dat |>
        dplyr::mutate(os_time = os_time) |>
        dplyr::mutate(observed = (time <= os_time)) |>
        dplyr::select(-os_time, -log_haz_link)

    assertthat::assert_that(
        length(unique(os_dat_complete$pt)) == n,
        n == nrow(os_dat_complete),
        all(os_dat_complete$time >= 0),
        all(os_dat_complete$event %in% c(0, 1)),
        nrow(lm_dat2) == n * length(bounds$time),
        all(!is.na(lm_dat2$observed))
    )
    return(list(os = os_dat_complete, lm = lm_dat2))
}
