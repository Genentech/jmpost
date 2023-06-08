# sim_lm_gsf ----

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
        mu_phi = c(0.4, 0.6),
        mu_b = 60,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1,
        omega_phi = 0.2,
        link_dsld = 0.5,
        link_ttg = 1
    ))
    expect_true(is.function(result))
    set.seed(123)
    fun_result <- result(df)
    expect_s3_class(fun_result, "data.frame")
    expect_identical(names(fun_result), c("pt", "time", "sld", "log_haz_link", "study", "arm"))
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
    expect_identical(names(fun_result), c("pt", "time", "sld", "log_haz_link", "study", "arm"))
    expect_identical(nrow(df), nrow(fun_result))
    expect_true(all(fun_result$sld > 0))
    expect_true(!any(duplicated(fun_result$sld)))
    expect_true(all(fun_result$log_haz_link != 0))
    expect_true(any(duplicated(fun_result$log_haz_link)))
})

# simulate_joint_data ----

test_that("simulate_joint_data works as expected", {
    set.seed(543)
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
            mu_phi = c(0.4, 0.6),
            mu_b = 60,
            omega_b = 0.1,
            omega_s = 0.1,
            omega_g = 0.1,
            omega_phi = 0.2,
            link_dsld = 0.5,
            link_ttg = 0.3
        ),
        os_fun = sim_os_weibull(
            lambda = 365 * (1 / 400),
            gamma = 1
        )
    ))
    expect_type(result, "list")
    expect_s3_class(result$os, "tbl_df")
    expect_identical(names(result$os), c("pt", "time", "cov_cont", "cov_cat", "event", "study", "arm"))
    expect_equal(nrow(result$os), 80 + 80)
    expect_s3_class(result$lm, "tbl_df")
    expect_identical(names(result$lm), c("pt", "time", "sld", "study", "arm", "observed"))
    expect_equal(nrow(result$lm), 97921) # calculated as sum(sapply(result$os$time, function(t) sum(times < t) + 1))
})
