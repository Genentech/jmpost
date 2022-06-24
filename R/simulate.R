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


SLD <- function(t, b, s, g, phi) {
    c1 <- b
    c2 <- phi * exp(-s * t)
    c3 <- (1 - phi) * exp(g * t)
    c1 * (c2 + c3)
}


dSLD <- function(t, b, s, g, phi) {
    c1 <- (1 - phi) * g * exp(g * t)
    c2 <- - phi * s * exp(-s * t)
    b * (c1 + c2)
}


TTG <- function(b, s, g, phi) {
    c1 <- log((s * phi) / (g * (1 - phi)))
    c2 <- g + s
    max(0, c1 / c2)
}


log_haz_loglogistic <- function(t, lambda, p) {
    c1 <- log(lambda) + log(p) + (p - 1) * log(lambda * t)
    c2 <- log(1 + (lambda * t)^p)
    c1 - c2
}




#' Simulate OS and SLD data
#'
#' @param N Number of patients to simulate
#' @param lambda_cen Lambda for censoring distribution (1/mean)
#' @param lambda Lambda for log-logistic baseline OS distribution
#' @param p p for log-logistic baseline OS distribution
#' @param beta link parameter for derivative SLD model
#' @param gamma link parameter for TTG
#' @param psi_mu_placebo SLD Kinetics parameters for the placebo group
#' @param psi_mu_trt SLD Kinetics parameters for the treatment group
#' @param psi_omega Variance for the SLD kinetrics parameters
#' @param eta_mu Mean for the random effects of the SLD parameters
#' @param eta_sigma Covariance for the random effects of the SLD parameters
#' @param beta_age Beta parameter for the age effect
#' @param beta_sex Beta parameters for sex effect
#' @param visits Visits in which SLD observations occoured at
#' @param time_gap Interval time gap to calculate hazard at. In years.
#' (small numbers = more
#' accurate distribution but longer run times to simulate data)
#' @param time_max Maximum time to calculate hazard at. In years. will error if patient
#' hasn't died before this time. Excessively large values will increase
#' the functions run time without adding value
#'
#' @import dplyr
#' @importFrom mvtnorm rmvnorm
#' @importFrom tidyr gather spread
#' @importFrom assertthat assert_that
#'
#' @export
simulate_os_sld <- function(
        N = 200,
        lambda_cen = 1 / 0.6,
        lambda = 0.9,
        p = 2,
        beta = 0.0035,
        gamma = -1.0,
        psi_mu_placebo = c(
            "b" = 58,
            "s" = 3,
            "g" = 0.5,
            "phi" = 0.2
        ),
        psi_mu_trt = c(
            "b" = 58,
            "s" = 3,
            "g" = 0.5,
            "phi" = 0.2
        ),
        psi_omega = list(
            "b" = 0.135,
            "s" = 0.15,
            "g" = 0.225,
            "phi" = 0.7
        ),
        eta_mu = rep(0, 4),
        eta_sigma = diag(rep(2, 4)),
        beta_age = 0.1,
        beta_sex = c("M" = 0, "F" = -0.3),
        visits = c(1, 10, 20, 40, 50),
        time_gap = 1 / 365,
        time_max = 4
) {
    unique_pts <- paste0("pt_",
                         sprintf(
                             paste0("%0", ceiling(log(N, base = 10)) + 1, "i"),
                             seq_len(N)
                         )
    )


    eta <- rmvnorm(N, eta_mu, eta_sigma)
    colnames(eta) <- names(psi_mu_trt)
    eta <- as_tibble(eta) %>% mutate(pt = unique_pts)

    trt_labels <- c(
        "PLACEBO",
        "TRT"
    )

    censoring <- tibble(pt = unique_pts) %>%
        mutate(time_cen = rexp(N, lambda_cen) * 365)



    blcovariates <- tibble(pt = unique_pts) %>%
        mutate(trt = sample(trt_labels, replace = TRUE, size = N)) %>%
        mutate(age = rnorm(N)) %>%
        mutate(sex = factor(
            sample(c("M", "F"), replace = TRUE, size = N),
            levels = c("M", "F")
        )) %>%
        mutate(log_haz_cov = age * beta_age + beta_sex[sex]) %>%
        mutate(lambda = lambda) %>%
        mutate(p = p) %>%
        mutate(survival = runif(N)) %>%
        mutate(chazard_limit = -log(survival))


    psi_mu_df <- tibble(
        var = c(names(psi_mu_placebo), names(psi_mu_trt)),
        mu = c(psi_mu_placebo, psi_mu_trt),
        trt = rep(trt_labels, each = length(psi_mu_placebo))
    )

    psi_omega_df <- tibble(
        var = names(psi_omega),
        omega = unlist(psi_omega)
    )


    sld_pars_meta <- eta %>%
        gather(var, eta, -pt) %>%
        left_join(select(blcovariates, pt, trt), by = "pt") %>%
        left_join(psi_mu_df, by = c("var", "trt")) %>%
        left_join(psi_omega_df, by = "var")


    sld_pars_phi <- sld_pars_meta %>%
        filter(var == "phi") %>%
        mutate(eta = plogis(qlogis(mu) + eta * omega))


    sld_pars_nphi <- sld_pars_meta %>%
        filter(var != "phi") %>%
        mutate(eta = exp(log(mu) + eta * omega))


    sld_pars <- bind_rows(sld_pars_nphi, sld_pars_phi) %>%
        select(-mu, -omega) %>%
        spread(var, eta)


    time_vals <- seq(time_gap, time_max, by = time_gap)


    dat <- expand.grid(pt = unique_pts, time = time_vals, stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        mutate(time_offset = time - (time_gap / 2)) %>%
        arrange(pt, time) %>%
        left_join(sld_pars, by = "pt") %>%
        mutate(SLD = SLD(time, b, s, g, phi)) %>%
        left_join(blcovariates, by = c("pt", "trt")) %>%
        mutate(ttg = TTG(b, s, g, phi)) %>%
        mutate(dslg = dSLD(time, b, s, g, phi)) %>%
        mutate(log_bl_haz = log_haz_loglogistic(time_offset, lambda, p)) %>%
        mutate(log_haz_dsld = beta * dslg) %>%
        mutate(log_haz_ttg = gamma * ttg) %>%
        mutate(hazard = exp(log_bl_haz + log_haz_cov + log_haz_dsld + log_haz_ttg)) %>%
        group_by(pt) %>%
        mutate(chazard = cumsum(hazard)) %>%
        filter(chazard <= chazard_limit) %>%
        mutate(time = time * 365) %>%
        group_by(pt) %>%
        mutate(final = row_number() == n()) %>%
        ungroup() %>%
        select(pt, time, SLD, age, sex, final) %>%
        arrange(pt, time)


    os_dat <- dat %>%
        filter(final) %>%
        left_join(censoring, by = "pt") %>%
        mutate(event = if_else(time <= time_cen, 1, 0)) %>%
        mutate(time = if_else(event == 1, time, time_cen)) %>%
        select(pt, time, age, sex, event)

    sld_dat <- dat %>%
        filter(time %in% visits) %>%
        arrange(pt, time) %>%
        left_join(censoring, by = "pt") %>%
        filter(time <= time_cen) %>%
        select(pt, time, SLD)

    assert_that(
        nrow(os_dat) == N,
        msg = "`time_max` is too small patients have been lost"
    )

    return(list(os = os_dat, sld = sld_dat))
}
