


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



# TODO - Need to enable code for link functions (currently implement as no-link)
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
    .debug = FALSE
) {
    function(lm_base) {
        
        assertthat::assert_that(
            length(unique(lm_base$study)) == 1,
            length(mu_s) == length(unique(lm_base$arm)),
            length(mu_s) == length(mu_g),
            length(mu_s) == length(mu_phi),
            length(mu_s) == length(mu_b),
            length(sigma) == 1,
            length(c(eta_b_sigma, eta_g_sigma, eta_phi_sigma, eta_s_sigma)) == 4,
            length(c(omega_b, omega_s, omega_g, omega_phi)) == 4
        )

        baseline_covs <- lm_base |>
            dplyr::distinct(pt, arm, study) |>
            dplyr::mutate(arm_n = as.numeric(factor(as.character(arm)))) |>
            dplyr::mutate(eta_b = rnorm(n(), 0, eta_b_sigma)) |>
            dplyr::mutate(eta_s = rnorm(n(), 0, eta_s_sigma)) |>
            dplyr::mutate(eta_g = rnorm(n(), 0, eta_g_sigma)) |>
            dplyr::mutate(eta_phi = rnorm(n(), 0, eta_phi_sigma)) |>
            dplyr::mutate(psi_b = exp(log(mu_b[arm_n]) + eta_b * omega_b)) |>
            dplyr::mutate(psi_s = exp(log(mu_s[arm_n]) + eta_s * omega_s)) |>
            dplyr::mutate(psi_g = exp(log(mu_g[arm_n]) + eta_g * omega_g)) |>
            dplyr::mutate(psi_phi = plogis(qlogis(mu_phi[arm_n]) + eta_phi * omega_phi))

        # TODO - time is currently converted to year scale for sld calculation
        #        as the psi default values are based on those provided by Daniels
        #        original code which was also on the year scale
        #        would be good to convert them to be on the day-scale
        lm_dat <- lm_base |>
            dplyr::select(-study, -arm) |>
            dplyr::left_join(baseline_covs, by = "pt") |>
            dplyr::mutate(mu_sld = sld(time / 365, psi_b, psi_s, psi_g, psi_phi)) |>
            dplyr::mutate(sld = rnorm(n(), mu_sld, mu_sld * sigma)) |>
            dplyr::mutate(log_haz_link = 0)

        if (!.debug) {
            lm_dat <- lm_dat |> dplyr::select(pt, time, sld, log_haz_link, study, arm)
        }
        return(lm_dat)
    }
}


# TODO - Need to be able to provide 1 slope per arm to enable a "treatment effect"
#' @export
sim_lm_random_slope <- function(z_sigma, intercept, slope, sigma, phi, .debug = FALSE) {
    function(lm_base) {
        rs_baseline <- lm_base |>
            dplyr::distinct(pt) |>
            dplyr::mutate(slope_ind = rnorm(n(), slope, sd = z_sigma))

        lm_dat <- lm_base |>
            dplyr::mutate(err = rnorm(n(), 0, sigma)) |>
            dplyr::left_join(rs_baseline, by = "pt") |>
            dplyr::mutate(sld = intercept + slope_ind * time + err) |>
            dplyr::mutate(log_haz_link = slope_ind * phi)
        
        if ( ! .debug) {
            lm_dat <- lm_dat |> select(pt, time, sld, log_haz_link, study, arm)
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
sim_os_loglogistic <- function(lambda, p){
    function(time) {
        c1 <- lambda * p * (lambda * time)^(p - 1)
        c2 <- 1 + (lambda * time)^p
        log(c1 / c2)
    }
}

# TODO - Update to enable multiple studys ? 
#' @export
simulate_joint_data <- function(
    n_arm = c(50, 80),   # Number of arms and number of subjects per arm
    max_time = 2000,
    lambda_cen = 1 / 3,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    lm_fun,
    os_fun
) {

    n <- sum(n_arm)
    u_pts <- sprintf("pt_%05i", seq_len(n))

    os_baseline <- dplyr::tibble(pt = u_pts) %>%
        dplyr::mutate(cov_cont = rnorm(n)) %>%
        dplyr::mutate(cov_cat = factor(
            sample(c("A", "B", "C"), replace = TRUE, size = n),
            levels = c("A", "B", "C")
        )) %>%
        dplyr::mutate(log_haz_cov = cov_cont * beta_cont + beta_cat[cov_cat]) %>%
        dplyr::mutate(survival = runif(n)) %>%
        dplyr::mutate(chazard_limit = -log(survival)) %>%
        dplyr::mutate(time_cen = rexp(n, lambda_cen)) |>
        dplyr::mutate(study = "Study-1") |>
        dplyr::mutate(arm = rep(paste0("Group-", seq_along(n_arm)), times = n_arm))
    
    lm_base <- expand.grid(time = 1:max_time, pt = u_pts, stringsAsFactors = FALSE) |>
        dplyr::as_tibble() |>
        dplyr::left_join(dplyr::select(os_baseline, pt, arm, study), by = "pt")
    
    lm_dat <- lm_fun(lm_base)

    os_dat <- expand.grid(time = as.double(1:max_time), pt = u_pts, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(time_offset = time - 0.5) %>%
        dplyr::left_join(os_baseline, by = "pt") %>%
        dplyr::left_join(dplyr::select(lm_dat, pt, time, log_haz_link), by = c("pt", "time")) %>%
        dplyr::mutate(log_bl_haz = os_fun(time_offset)) %>%
        dplyr::mutate(hazard_per_interval = exp(log_bl_haz + log_haz_cov + log_haz_link)) %>%
        dplyr::group_by(pt) %>%
        dplyr::mutate(chazard = cumsum(hazard_per_interval)) %>%
        dplyr::filter(chazard <= chazard_limit) |>
    dplyr::select(pt, time, cov_cont, cov_cat, time_cen, study, arm) |>
        dplyr::arrange(pt, desc(time)) |>
        dplyr::group_by(pt) %>%
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = dplyr::if_else(time <= time_cen, 1, 0)) %>%
        dplyr::mutate(time = dplyr::if_else(event == 1, time, time_cen)) %>%
        dplyr::select(pt, time, cov_cont, cov_cat, event, study, arm)

    missing_pts <- setdiff(u_pts, unique(os_dat$pt))
    message(sprintf("%d people died before day 1", length(missing_pts)))

    lm_dat2 <- lm_dat |>
        dplyr::inner_join(dplyr::select(os_dat, pt, os_time = time), by = "pt") |>
        dplyr::mutate(observed = (time <= os_time)) |>
        dplyr::mutate(observed = if_else(is.na(observed), FALSE, observed)) |>
        dplyr::select(-os_time, -log_haz_link)

    assertthat::assert_that(nrow(os_dat) == (n - length(missing_pts)))
    assertthat::assert_that(nrow(lm_dat) == n * max_time)

    return(list(os = os_dat, lm = lm_dat2))
}



# dat <- simulate_os_lm(
#     n = 500,
#     max_time = 2000,
#     lambda_cen = 1/9000,
#     beta_cont = 0.2,
#     beta_cat = c(
#         "A" = 0,
#         "B" = -0.3,
#         "C" = 0.4
#     ),
#     lm_fun = lm_random_slope(
#         intercept = 30,
#         sigma = 3,
#         slope = 0.3,
#         z_sigma = 0.02,
#         phi = 0.1
#     ),
#     os_fun = os_haz_weibull(lambda = 1/250, gamma=1)
# )

