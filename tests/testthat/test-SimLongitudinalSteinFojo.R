
test_that("SimLongitudinalSteinFojo works as expected", {
    sim <- SimLongitudinalSteinFojo(
        times = c(-100, 0, 50),
        sigma = 0.00000001,
        mu_b = c(60, 90),
        mu_s = c(0.6, 0.4),
        mu_g = c(0.25, 0.35),
        omega_b = 0.000000001,
        omega_s = 0.000000001,
        omega_g = 0.000000001,
        link_dsld = 0,
        link_ttg = 0,
        link_identity = 0
    )
    expect_true(is(sim, "SimLongitudinalSteinFojo"))

    subs <- dplyr::tibble(
        pt = c("1", "2", "3"),
        arm = factor(c("A", "B", "A")),
        study = factor(c("study1", "study1", "study2"))
    )
    res_subs <- sampleSubjects(sim, subs)
    expect_equal(res_subs$pt, subs$pt)
    expect_equal(res_subs$arm, subs$arm)
    expect_equal(res_subs$study, subs$study)
    expect_equal(res_subs$psi_b, c(60, 60, 90), tolerance = 0.00001)
    expect_equal(res_subs$psi_s, c(0.6, 0.4, 0.6), tolerance = 0.00001)
    expect_equal(res_subs$psi_g, c(0.25, 0.35, 0.25), tolerance = 0.0001)
    expect_equal(nrow(res_subs), nrow(subs))
    expect_equal(
        names(res_subs),
        c("pt", "arm", "study", "psi_b", "psi_s", "psi_g")
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
            "pt", "arm", "study", "psi_b", "psi_s", "psi_g", "time",
            "mu_sld", "dsld", "ttg", "sld", "log_haz_link"
        )
    )
})

test_that("print methods work as expected", {
    expect_snapshot(print(SimLongitudinalSteinFojo()))
})
