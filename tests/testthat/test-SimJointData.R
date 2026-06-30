test_that("SimJointData works as expected", {
    set.seed(5433)
    result <- expect_silent({
        SimJointData(
            design = list(
                SimGroup(50, "Arm-A", "Study-X"),
                SimGroup(80, "Arm-B", "Study-X")
            ),
            survival = SimSurvivalWeibullPH(
                time_max = 3,
                time_step = 1 / 365,
                lambda_censor = 1 / 9000,
                lambda = 1 / 200 * 365,
                gamma = 0.95,
                beta_cont = 0.3,
                beta_cat = c("A" = 0, "B" = -0.1, "C" = 0.5)
            ),
            longitudinal = SimLongitudinalGSF(
                sigma = 0.003,
                mu_s = log(c(0.2, 0.25)),
                mu_g = log(c(0.15, 0.2)),
                mu_b = log(60),
                mu_phi = qlogis(c(0.4, 0.6)),
                omega_b = 0.1,
                omega_s = 0.1,
                omega_g = 0.1,
                omega_phi = 0.1,
                link_dsld = 0,
                link_ttg = 0,
                scaled_variance = TRUE
            ),
            .silent = TRUE
        )
    })

    expect_class(result, "SimJointData")

    expect_s3_class(result@survival, "tbl_df")
    expect_set_equal(
        names(result@survival),
        c("subject", "study", "arm", "time", "event", "cov_cont", "cov_cat")
    )
    expect_equal(nrow(result@survival), 50 + 80)

    expect_s3_class(result@longitudinal, "tbl_df")
    expect_set_equal(
        names(result@longitudinal),
        c("subject", "arm", "study", "time", "sld", "observed")
    )

    lm_observed_after_death <- result@longitudinal |>
        dplyr::filter(.data$observed) |>
        dplyr::select(subject, time) |>
        dplyr::left_join(
            dplyr::select(result@survival, subject, stime = time),
            by = "subject"
        ) |>
        dplyr::filter(.data$time > .data$stime)

    expect_equal(nrow(lm_observed_after_death), 0)
})


test_that("SimJointData leads to valid DataJoint with almost only default arguments", {
    set.seed(321)
    sim_data <- SimJointData(
        longitudinal = SimLongitudinalGSF(),
        survival = SimSurvivalExponential(
            time_max = 4,
            lambda = 365 / 100,
            time_step = 1 / 365
        ),
        .silent = TRUE
    )

    joint_data <- DataJoint(
        subject = DataSubject(
            data = sim_data@survival,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = sim_data@survival,
            formula = Surv(time, event) ~ cov_cat + cov_cont
        ),
        longitudinal = DataLongitudinal(
            data = sim_data@longitudinal,
            formula = sld ~ time,
            threshold = 5
        )
    )
    expect_true(validObject(joint_data))
})


test_that("print methods work as expected", {
    expect_snapshot({
        sim_data <- SimJointData(
            longitudinal = SimLongitudinalGSF(),
            survival = SimSurvivalExponential(
                time_max = 4,
                lambda = 365 / 100,
                time_step = 1 / 365
            ),
            .silent = TRUE
        )
        print(sim_data)
    })
})

test_that("add_pfs works as expected", {
    set.seed(5433)
    sim_data <- SimJointData(
        longitudinal = SimLongitudinalGSF(
            mu_g = log(c(0.5, 0.35)),
            mu_s = log(c(0.001, 0.35)),
            scaled_variance = TRUE
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            lambda = 365 / 100,
            time_step = 1 / 365
        ),
        .silent = TRUE
    )
    result <- add_pfs(sim_data)
    expect_true(all(result@survival$pfs_time <= result@survival$time))
    expect_true(all(result@survival$pfs_event >= result@survival$event))
    expect_equal(mean(result@survival$time), 0.33969863)
    expect_equal(mean(result@survival$pfs_time), 0.302575342)

    expect_equal(
        result@longitudinal$observed[
            result@longitudinal$subject == "subject_027"
        ],
        rep(c(TRUE, FALSE), times = c(7, 3))
    )

    result_obs_after <- add_pfs(sim_data, observed_after = TRUE)
    expect_equal(
        result_obs_after@longitudinal$observed[
            result_obs_after@longitudinal$subject == "subject_027"
        ],
        rep(c(TRUE, FALSE), times = c(10, 0))
    )
})

test_that("add_pfs works with large from_time", {
    set.seed(5433)
    sim_data <- SimJointData(
        longitudinal = SimLongitudinalGSF(
            mu_g = log(c(0.5, 0.35)),
            mu_s = log(c(0.001, 0.35)),
            scaled_variance = TRUE
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            lambda = 365 / 100,
            time_step = 1 / 365
        ),
        .silent = TRUE
    )
    result <- add_pfs(sim_data, from_time = 1000)
    expect_equal(result@survival$time, result@survival$pfs_time)
})


test_that("cut_data works as expected", {
    set.seed(123)
    sim_data <- SimJointData(
        longitudinal = SimLongitudinalGSF(scaled_variance = TRUE),
        survival = SimSurvivalExponential(
            time_max = 4,
            lambda = 365 / 100,
            time_step = 1 / 365
        ),
        .silent = TRUE
    )

    result <- cut_data(sim_data, cut_time = 1)

    expect_equal(max(sim_data@longitudinal$time), 1.50684932)
    expect_equal(max(result@longitudinal$time), 0.9589041)
    # == max(result@longitudinal$time[result@longitudinal$time <= 1])

    expect_error(
        cut_data(sim_data, cut_time = 1:4),
        "have length 100"
    )

    per_pat_cut <- seq(0.0001, 0.002, length = 100)
    result_pat_cuts <- cut_data(sim_data, cut_time = per_pat_cut)
    expect_equal(result_pat_cuts@survival$time, per_pat_cut)
    expect_equal(result_pat_cuts@survival$event, rep(0, 100))
})
