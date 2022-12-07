

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



sim_lm_random_slope <- function(z_sigma, intercept, slope, sigma, phi) {
    function(u_pts, max_time) {
        n <- length(u_pts)

        rs_baseline <- dplyr::tibble(
            pt = u_pts,
            rslope = rnorm(n, 0, sd = z_sigma)
        )

        lm_dat <- expand.grid(time = 1:max_time, pt = u_pts, stringsAsFactors = FALSE) |>
            dplyr::as_tibble() |>
            dplyr::mutate(val = rnorm(n * max_time, 0, sigma)) |>
            dplyr::left_join(rs_baseline, by = "pt") |>
            dplyr::mutate(outcome = intercept + val + (rslope * time) + (slope * time)) |>
            dplyr::mutate(log_haz_link = rslope * phi) |>
            dplyr::select(pt, time, outcome, log_haz_link, rslope)
        
        return(lm_dat)
    }
}

sim_os_weibull <- function(lambda, gamma) {
    function(time) {
        log(lambda) + log(gamma) + (gamma - 1) * log(time)
    }
}

sim_os_loglogistic <- function(lambda, p){
    function(time) {
        c1 <- lambda * p * (lambda * time)^(p - 1)
        c2 <- 1 + (lambda * time)^p
        log(c1 / c2)
    }
}


simulate_joint_data <- function(
    n = 200,
    max_time = 2000,
    lambda_cen = 1 / 3,
    beta_cont = 0.2,
    beta_cat = c("A" = 0, "B" = -0.4, "C" = 0.2),
    lm_fun,
    os_fun
) {

    u_pts <- sprintf("pt_%05i", 1:n)

    lm_dat <- lm_fun(u_pts, max_time)

    os_baseline <- dplyr::tibble(pt = u_pts) %>%
        dplyr::mutate(cov_cont = rnorm(n)) %>%
        dplyr::mutate(cov_cat = factor(
            sample(c("A", "B", "C"), replace = TRUE, size = n),
            levels = c("A", "B", "C")
        )) %>%
        dplyr::mutate(log_haz_cov = cov_cont * beta_cont + beta_cat[cov_cat]) %>%
        dplyr::mutate(survival = runif(n)) %>%
        dplyr::mutate(chazard_limit = -log(survival)) %>%
        dplyr::mutate(time_cen = rexp(n, lambda_cen))

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
        dplyr::select(pt, time, cov_cont, cov_cat, time_cen) |>
        dplyr::arrange(pt, desc(time)) |>
        dplyr::group_by(pt) %>%
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(event = dplyr::if_else(time <= time_cen, 1, 0)) %>%
        dplyr::mutate(time = dplyr::if_else(event == 1, time, time_cen)) %>%
        dplyr::select(pt, time, cov_cont, cov_cat, event)

    missing_pts <- setdiff(u_pts, unique(os_dat$pt))
    
    died_day_1 <- dplyr::tibble(
        pt = missing_pts,
        time = 1,
        event = 1
    ) |>
        dplyr::left_join(dplyr::select(os_baseline, pt, cov_cont, cov_cat), by = "pt")
    
    os_dat2 <- os_dat |>
        dplyr::bind_rows(died_day_1)

    lm_dat2 <- lm_dat |>
        dplyr::left_join(dplyr::select(os_dat2, pt, os_time = time), by = "pt") |>
        dplyr::mutate(observed = (time <= os_time)) |>
        dplyr::select(-os_time, -log_haz_link)

    assertthat::assert_that(nrow(os_dat2) == n)
    assertthat::assert_that(nrow(lm_dat) == n * max_time)

    return(list(os = os_dat2, lm = lm_dat2))
}



# ## Generate Test data (can just ignore the OS data for now)
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

