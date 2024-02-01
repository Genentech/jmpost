# sim_lm_gsf ----

library(survival)

test_that("sim_lm_gsf works as expected", {
    df <- data.frame(
        pt = rep(c("1", "2", "3"), each = 5),
        time = rep(1:5, 3),
        arm = rep(c("A", "B"), c(1, 2) * 5),
        study = "study1"
    )
    result <- expect_silent(sim_lm_gsf(
        sigma = 0.003,
        mu_s = c(0.2, 0.25),
        mu_g = c(0.15, 0.2),
        mu_b = 60,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1,
        a_phi = c(4, 6),
        b_phi = c(4, 6),
        link_dsld = 0.5,
        link_ttg = 1
    ))
    expect_true(is.function(result))
    set.seed(123)
    fun_result <- result(df)
    expect_s3_class(fun_result, "data.frame")
    expect_true(all(c("pt", "time", "sld", "log_haz_link", "study", "arm") %in% names(fun_result)))
    expect_identical(nrow(df), nrow(fun_result))
    expect_true(all(fun_result$sld > 0))
    expect_true(!any(duplicated(fun_result$sld)))
    expect_true(all(fun_result$log_haz_link > 0))
    expect_true(!any(duplicated(fun_result$log_haz_link)))
})

# sim_lm_random_slope ----

test_that("sim_lm_random_slope works as expected", {
    df <- data.frame(
        pt = rep(c("1", "2", "3"), each = 5),
        time = rep(1:5, 3),
        arm = rep(c("A", "B"), c(1, 2) * 5),
        study = "study1"
    )
    result <- expect_silent(sim_lm_random_slope(
        intercept = 50,
        slope_mu = c(0.01, 0.03),
        slope_sigma = 0.5,
        sigma = 2,
        phi = 0.1
    ))
    expect_true(is.function(result))
    set.seed(123)
    fun_result <- result(df)
    expect_s3_class(fun_result, "data.frame")
    expect_true(all(c("pt", "time", "sld", "log_haz_link", "study", "arm") %in% names(fun_result)))
    expect_identical(nrow(df), nrow(fun_result))
    expect_true(all(fun_result$sld > 0))
    expect_true(!any(duplicated(fun_result$sld)))
    expect_true(all(fun_result$log_haz_link != 0))
    expect_true(any(duplicated(fun_result$log_haz_link)))
})

# simulate_joint_data ----

test_that("simulate_joint_data works as expected", {
    set.seed(5433)
    times <- seq(0, 4, by = (1 / 365) / 2)
    result <- expect_silent(simulate_joint_data(
        n_arm = c(80, 80),
        times = times,
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3,
        lm_fun = sim_lm_gsf(
            sigma = 0.003,
            mu_s = c(0.2, 0.25),
            mu_g = c(0.15, 0.2),
            mu_b = 60,
            omega_b = 0.1,
            omega_s = 0.1,
            omega_g = 0.1,
            a_phi = c(4, 6),
            b_phi = c(4, 6),
            link_dsld = 0,
            link_ttg = 0
        ),
        os_fun = sim_os_weibull(
            lambda = 1 / 200 * 365,
            gamma = 0.97
        )
    ))
    expect_type(result, "list")
    expect_s3_class(result$os, "tbl_df")
    expect_identical(
        names(result$os),
        c("pt", "time", "event", "cov_cont", "cov_cat", "study", "arm")
    )
    expect_equal(nrow(result$os), 80 + 80)
    expect_s3_class(result$lm, "tbl_df")
    expect_identical(
        names(result$lm),
        c("pt", "time", "sld", "study", "arm", "observed")
    )
    expect_equal(
        nrow(result$lm |> dplyr::filter(.data$observed)),
        sum(sapply(result$os$time, function(t) sum(times < t) + 1))
    )
})

test_that("simulate_joint_data leads to valid DataJoint with almost only default arguments", {
    set.seed(321)
    sim_data <- simulate_joint_data(
        lm_fun = sim_lm_random_slope(),
        os_fun = sim_os_exponential(lambda = 1 / 100)
    )
    os_data <- sim_data$os
    long_data <- sim_data$lm |>
        dplyr::arrange(time, pt)

    joint_data <- DataJoint(
        subject = DataSubject(
            data = os_data,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = os_data,
            formula = Surv(time, event) ~ cov_cat + cov_cont
        ),
        longitudinal = DataLongitudinal(
            data = long_data,
            formula = sld ~ time,
            threshold = 5
        )
    )
    expect_true(validObject(joint_data))
})


test_that("sim_os_exponential creates a dataset with the correct parameter", {
    set.seed(321)
    sim_data <- simulate_joint_data(
        n_arm = c(200, 200),
        times = seq(1, 1000),
        beta_cont = 0,
        beta_cat = c("A" = 0, "B" = 0, "C" = 0),
        lm_fun = sim_lm_random_slope(phi = 0),
        os_fun = sim_os_exponential(lambda = 1 / 100)
    )
    osdat <- sim_data$os
    mod <- survreg(Surv(time, event) ~ 1, data = osdat, dist = "exponential")

    par_real <- -log(1 / 100)
    par_obs <- coef(mod)[1]
    par_obs_se <- sqrt(vcov(mod)[1, 1])
    z_score <- (par_obs - par_real) / par_obs_se
    z_score
    expect_true(abs(z_score) < qnorm(0.7))
})


test_that("sim_os_weibull creates a dataset with the correct parameter", {
    set.seed(2434)
    lambda_real <- 1 / 100
    gamma_real <- 0.9
    sim_data <- simulate_joint_data(
        n_arm = c(200, 200),
        times = seq(1, 1000),
        beta_cont = 0.2,
        beta_cat = c("A" = 0, "B" = -0.6, "C" = 0.6),
        lm_fun = sim_lm_random_slope(phi = 0),  # No link
        os_fun = sim_os_weibull(lambda = lambda_real, gamma = gamma_real),
        .silent = TRUE
    )
    osdat <- sim_data$os
    mod <- survreg(Surv(time, event) ~ cov_cont + cov_cat, data = osdat, dist = "weibull")
    scale <- mod$scale

    ## Check covariate coefficients
    alpha_real <- -c(0.2, -0.6, 0.6) * scale  # Convert from PH to scale-location formulation
    alpha_obs <- coef(mod)[-1]
    alpha_se <- sqrt(diag(vcov(mod)[2:4, 2:4]))
    z_score <- (alpha_obs - alpha_real) / alpha_se
    expect_true(all(abs(z_score) < qnorm(0.9)))

    ## Check Lambda parameter
    int_real <- -log(lambda_real) * (scale)
    int_obs <- coef(mod)[1]
    int_obs_se <- sqrt(vcov(mod)[1, 1])
    z_score <- (int_real - int_obs) / int_obs_se
    expect_true(abs(z_score) < qnorm(0.9))

    ## Check Scale Parameter
    log_scale_real <- log(1 / gamma_real)
    log_scale_obs <- log(scale)
    log_scale_se <- sqrt(vcov(mod)["Log(scale)", "Log(scale)"])
    z_score <- (log_scale_obs - log_scale_real) / log_scale_se
    expect_true(abs(z_score) < qnorm(0.9))
})
