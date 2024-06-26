
test_that("SimLongitudinalClaretBruno works as expected", {
    sim <- SimLongitudinalClaretBruno(
        times = c(-100, 0, 50),
        sigma = 0.00000001,
        mu_b = log(c(60, 90)),
        mu_g = log(c(0.6, 0.4)),
        mu_c = log(c(0.25, 0.35)),
        mu_p = log(c(1, 1.5)),
        omega_b = 0.000000001,
        omega_g = 0.000000001,
        omega_c = 0.000000001,
        omega_p = 0.000000001,
        link_dsld = 0,
        link_ttg = 0,
        link_identity = 0
    )
    expect_true(is(sim, "SimLongitudinalClaretBruno"))

    subs <- dplyr::tibble(
        subject = c("1", "2", "3"),
        arm = factor(c("A", "B", "A")),
        study = factor(c("study1", "study1", "study2"))
    )
    res_subs <- sampleSubjects(sim, subs)
    expect_equal(res_subs$subject, subs$subject)
    expect_equal(res_subs$arm, subs$arm)
    expect_equal(res_subs$study, subs$study)
    expect_equal(res_subs$ind_b, c(60, 60, 90), tolerance = 0.00001)
    expect_equal(res_subs$ind_g, c(0.6, 0.4, 0.6), tolerance = 0.00001)
    expect_equal(res_subs$ind_c, c(0.25, 0.35, 0.25), tolerance = 0.0001)
    expect_equal(res_subs$ind_p, c(1, 1.5, 1), tolerance = 0.0001)
    expect_equal(nrow(res_subs), nrow(subs))
    expect_equal(
        names(res_subs),
        c("subject", "arm", "study", "ind_b", "ind_g", "ind_c", "ind_p")
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
    expect_equal(res_obvs$subject, tdat$subject)
    expect_equal(res_obvs$arm, tdat$arm)
    expect_equal(res_obvs$study, tdat$study)
    expect_equal(res_obvs$time, tdat$time)
    expect_equal(nrow(res_obvs), nrow(tdat))
    expect_equal(
        names(res_obvs),
        c(
            "subject", "arm", "study", "ind_b", "ind_g", "ind_c", "ind_p", "time",
            "mu_sld", "dsld", "ttg", "sld", "log_haz_link"
        )
    )
})

test_that("print methods work as expected", {
    expect_snapshot(print(SimLongitudinalClaretBruno()))
})
