
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
            .silent = TRUE
        )
    })

    expect_class(result, "SimJointData")

    expect_s3_class(result@survival, "tbl_df")
    expect_identical(
        names(result@survival),
        c("subject", "study", "arm", "time", "event", "cov_cont", "cov_cat")
    )
    expect_equal(nrow(result@survival), 50 + 80)


    expect_s3_class(result@longitudinal, "tbl_df")
    expect_identical(
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
        survival = SimSurvivalExponential(time_max = 4, lambda = 365 / 100, time_step =  1 / 365),
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
            survival = SimSurvivalExponential(time_max = 4, lambda = 365 / 100, time_step = 1 / 365),
            .silent = TRUE
        )
        print(sim_data)
    })
})
