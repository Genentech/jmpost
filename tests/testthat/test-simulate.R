if (is_full_test()) {
    subj_df <- os_data |>
        dplyr::mutate(study = "OAK") |>
        dplyr::select(study, id, arm)

    subj_data <- DataSubject(
        data = subj_df,
        subject = "id",
        arm = "arm",
        study = "study"
    )

    long_df <- tumor_data |>
        dplyr::select(id, year, sld)
    long_data <- DataLongitudinal(
        data = long_df,
        formula = sld ~ year
    )

    surv_data <- DataSurvival(
        data = os_data,
        formula = Surv(os_time, os_event) ~ ecog + age + race + sex
    )

    joint_data <- DataJoint(
        subject = subj_data,
        longitudinal = long_data,
        survival = surv_data
    )
    joint_mod <- JointModel(
        longitudinal = LongitudinalSteinFojo(
            mu_bsld = prior_normal(log(65), 1),
            mu_ks = prior_normal(log(0.52), 1),
            mu_kg = prior_normal(log(1.04), 1),
            omega_bsld = prior_normal(0, 3) |> set_limits(0, Inf),
            omega_ks = prior_normal(0, 3) |> set_limits(0, Inf),
            omega_kg = prior_normal(0, 3) |> set_limits(0, Inf),
            sigma = prior_normal(0, 3) |> set_limits(0, Inf)
        ),
        survival = SurvivalWeibullPH(
            lambda = prior_gamma(0.7, 1),
            gamma = prior_gamma(1.5, 1),
            beta = prior_normal(0, 3)
        ),
        link = linkGrowth(
            prior = prior_normal(0, 3)
        )
    )

    options("jmpost.prior_shrinkage" = 0.9)
    set.seed(19919)
    joint_results <- sampleStanModel(
        joint_mod,
        data = joint_data,
        iter_sampling = 500,
        iter_warmup = 500,
        chains = 2,
        parallel_chains = 1,
        step_size = 0.01,
        seed = 1
    )

}


test_that("simulate works with default options", {
    skip_if_not(is_full_test())
    set.seed(12345)
    results <- simulate(joint_results)
    expect_data_frame(
        results@survival,
        nrow = 203,
        ncols = 9
    )

    expect_equal(
        joint_data@survival@data[c("arm", "age", "ecog", "sex")],
        results@survival[c("arm", "age", "ecog", "sex")]
    )
    expect_equal(mean(results@survival$time), 2.58620690)
    mean(results@survival$event, 1)

    expect_data_frame(
        results@longitudinal,
        nrow = 1015,
        ncols = 6
    )
    expect_equal(
        mean(results@longitudinal[results@longitudinal$observed, ]$sld),
        37.9220115
    )
})

test_that("simulate works with jitter and times", {
    skip_if_not(is_full_test())
    set.seed(12345)
    results <- simulate(joint_results, jitter_var = c(0, 1), times = c(-10, 10))
    expect_data_frame(
        results@survival,
        nrow = 203,
        ncols = 9
    )

    expect_data_frame(
        results@longitudinal,
        nrow = 406,
        ncols = 6
    )
    expect_numeric(results@longitudinal$time[results@longitudinal$time < 0], lower = -10, upper = -10, len = 203)
    expect_numeric(results@longitudinal$time[results@longitudinal$time > 0], lower = 7, upper = 13, len = 203)
    expect_numeric(mean(results@longitudinal$time[results@longitudinal$time > 0]), lower = 9.9, upper = 10.1)

    expect_equal(
        mean(results@longitudinal[results@longitudinal$observed, ]$sld),
        2.885339
    )
})

test_that("simulate works with lambda_censor", {
    skip_if_not(is_full_test())
    set.seed(12345)
    results <- simulate(joint_results, times = (1:10), lambda_censor = 1 / 6, time_step = 0.5)
    expect_data_frame(
        results@survival,
        nrow = 203,
        ncols = 9
    )

    expect_equal(mean(results@survival$event), 0.65517241)
    expect_equal(mean(results@survival$time), 1.74296455)
    expect_equal(mean(results@survival$time[results@survival$event == 0]), 1.597454352)
    expect_equal(mean(results@survival$time[results@survival$event == 1]), 1.819548872)

    joined <- dplyr::left_join(
        results@survival[, c("subject", "time", "event")],
        results@longitudinal[, c("subject", "time", "observed")],
        by = "subject"
    )

    expect_true(all(!joined$observed[joined$time.y > joined$time.x]))
    expect_true(all(joined$observed[joined$time.y <= joined$time.x]))
})
