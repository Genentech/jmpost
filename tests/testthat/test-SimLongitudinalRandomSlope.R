
test_that("SimLongitudinalRandomSlope works as expected", {
    sim <- SimLongitudinalRandomSlope(
        times = c(-100, 0, 50),
        intercept = c(100, 50),
        slope_mu = c(10, 30),
        slope_sigma = 0.0000001,
        sigma = 2,
        link_dsld = 0,
        link_identity = 0
    )
    expect_true(is(sim, "SimLongitudinalRandomSlope"))

    subs <- dplyr::tibble(
        pt = c("1", "2", "3"),
        arm = factor(c("A", "B", "A")),
        study = factor(c("study1", "study1", "study2"))
    )
    res_subs <- sampleSubjects(sim, subs)
    expect_equal(res_subs$pt, subs$pt)
    expect_equal(res_subs$arm, subs$arm)
    expect_equal(res_subs$study, subs$study)
    expect_equal(res_subs$intercept, c(100, 100, 50))
    expect_equal(res_subs$slope_ind, c(10, 30, 10), tolerance = 0.0000001)
    expect_equal(nrow(res_subs), nrow(subs))
    expect_equal(
        names(res_subs),
        c("pt", "arm", "study", "intercept", "slope_ind")
    )


    tdat <- purrr::map(
        sim@times,
        \(time) {
            res_subs$time <- time
            res_subs
        }
    ) |>
        dplyr::bind_rows()

    res_obvs <- sampleObservations(sim, tdat)
    expect_equal(res_obvs$pt, tdat$pt)
    expect_equal(res_obvs$arm, tdat$arm)
    expect_equal(res_obvs$study, tdat$study)
    expect_equal(res_obvs$time, tdat$time)
    expect_equal(nrow(res_obvs), nrow(tdat))
    expect_equal(
        names(res_obvs),
        c(
            "pt", "arm", "study", "intercept", "slope_ind", "time",
            "err", "sld_mu", "sld", "log_haz_link"
        )
    )
    expect_equal(
        res_obvs$sld_mu + res_obvs$err,
        res_obvs$sld
    )
})

test_that("SimLongitudinalRandomSlope correctly generates a dataset with known parameters", {
    set.seed(3521)
    sim_data <- SimJointData(
        design = list(
            SimGroup(200, "Arm-A", "Study-X"),
            SimGroup(200, "Arm-A", "Study-Y")
        ),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(-100, -50, -10, 0, 10, 50, 150, 200, 250, 301, 425, 532),
            intercept = c(50, 70),
            slope_mu = 10,
            slope_sigma = 0.5,
            sigma = 2,
            link_dsld = 0,
            link_identity = 0
        ),
        survival = SimSurvivalExponential(lambda = 1 / 100, time_max = 10, time_step = 1),
        .silent = TRUE
    )

    mod <- lme4::lmer(
        dat = sim_data@longitudinal,
        formula = sld ~ 0 + study + time + (0 + time | pt),
    )

    ests <- lme4::fixef(mod)
    ests_se <- sqrt(diag(as.matrix(vcov(mod))))
    z_score <- (ests - c(50, 70, 10)) / ests_se
    expect_true(all(abs(z_score) < qnorm(0.99)))
})
