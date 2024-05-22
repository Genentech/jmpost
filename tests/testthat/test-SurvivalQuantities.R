


test_that("SurvivalQuantities and autoplot.SurvivalQuantities works as expected", {
    ensure_test_data_1()

    expected_column_names <- c("group", "time", "median", "lower", "upper")

    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(
            groups = list("a" = c("subject_0001", "subject_0002")),
            times = c(10, 20, 200, 300)
        )
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4)
    expect_equal(length(unique(preds$group)), 1)
    expect_equal(names(preds), expected_column_names)
    expect_equal(survsamps@type, "surv")


    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(times = c(10, 20, 200, 300))
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4 * nrow(test_data_1$dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), test_data_1$dat_os$subject)


    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(groups = list("subject_0001" = "subject_0001", "subject_0003" = "subject_0003"))
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)
    expect_equal(max(preds$time), max(test_data_1$dat_os$time))

    # Check that the relationship between the quantitites is preservered e.g.
    # that `surv = exp(-cumhaz)`
    preds1 <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(subjects = "subject_0001",  times = c(200, 300))
    ) |> summary()
    preds2 <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(subjects = "subject_0001",  times = c(200, 300)),
        type = "cumhaz"
    ) |> summary()
    preds3 <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(subjects = "subject_0001",  times = c(200, 300)),
        type = "haz"
    ) |> summary()
    preds4 <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(subjects = "subject_0001",  times = c(200, 300)),
        type = "loghaz"
    ) |> summary()
    expect_equal(round(preds1$median, 5), round(exp(-preds2$median), 5))
    expect_equal(round(preds3$median, 5), round(exp(preds4$median), 5))
    expect_true(all(preds1$median != preds3$median))
})



test_that("autoplot.SurvivalSamples works as expected", {
    samps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridFixed(subjects = c("subject_0011", "subject_0061"))
    )
    p <- autoplot(
        samps,
        add_wrap = FALSE,
        conf.level = FALSE
    )
    dat <- summary(samps)
    expect_length(p$layers, 1)
    expect_equal(p$labels$y, expression(S(t)))
    expect_true(inherits(p$facet, "FacetNull"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))


    samps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(
            groups = list("a" = c("subject_0011", "subject_0061"), "b" = c("subject_0001", "subject_0002")),
            times = c(10, 20, 50, 200)
        ),
        type = "loghaz"
    )
    p <- autoplot(samps, add_wrap = TRUE)
    dat <- summary(samps)
    expect_length(p$layers, 2)
    expect_equal(p$labels$y, expression(log(h(t))))
    expect_true(inherits(p$facet, "FacetWrap"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomRibbon"))


    set.seed(39130)
    subjectgroups <- list(
        gtsubject1 = sample(test_data_1$dat_os$subject, 20),
        gtsubject2 = sample(test_data_1$dat_os$subject, 20),
        gtsubject3 = sample(test_data_1$dat_os$subject, 20)
    )
    times <- seq(0, 100, by = 10)
    samps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(
            groups = subjectgroups,
            times = times
        ),
        type = "surv"
    )
    p <- autoplot(
        samps,
        conf.level = NULL,
        add_wrap = FALSE,
        add_km = TRUE
    )
    dat <- summary(samps)
    expect_length(p$layers, 3)
    expect_equal(p$labels$y, expression(S(t)))
    expect_true(inherits(p$facet, "FacetNull"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomKm"))
    expect_true(inherits(p$layers[[3]]$geom, "GeomKmTicks"))
})



test_that("SurvivalQuantities print method works as expected", {
    expect_snapshot({
        set.seed(3219)
        subjectgroups <- list(
            gtsubject1 = sample(test_data_1$dat_os$subject, 20),
            gtsubject2 = sample(test_data_1$dat_os$subject, 20),
            gtsubject3 = sample(test_data_1$dat_os$subject, 20)
        )
        times <- seq(0, 100, by = 10)
        samps_p1 <- SurvivalQuantities(
            test_data_1$jsamples,
            grid = GridGrouped(
                groups = subjectgroups,
                times = times
            ),
            type = "surv"
        )
        print(samps_p1)
    })

    expect_snapshot({
        times <- seq(0, 100, by = 10)
        samps_p2 <- SurvivalQuantities(
            test_data_1$jsamples,
            grid = GridFixed(
                times = times
            ),
            type = "loghaz"
        )
        print(samps_p2)
    })
})


test_that("SurvivalQuantities() works with time = 0", {
    ensure_test_data_1()

    expect_error(
        {
            SurvivalQuantities(
                test_data_1$jsamples,
                grid = GridGrouped(
                    groups = list("a" = c("subject_0001", "subject_0002")),
                    times = c(-10, 0, 10, 20, 200, 300)
                )
            )
        },
        regexp = "must be greater than or equal to 0"
    )

    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(
            groups = list("a" = c("subject_0001", "subject_0002")),
            times = c(0, 10, 20)
        )
    )
    preds <- summary(survsamps)
    # Time 0 should result in certainty of survival = 1
    expect_equal(preds$median[1], 1)
    expect_equal(preds$lower[1], 1)
    expect_equal(preds$upper[1], 1)


    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        grid = GridGrouped(
            groups = list("a" = c("subject_0001", "subject_0002")),
            times = c(0, 10, 20)
        ),
        type = "cumhaz"
    )
    preds <- summary(survsamps)
    # Time 0 should result in certainty of cumhaz = 0
    expect_equal(preds$median[1], 0)
    expect_equal(preds$lower[1], 0)
    expect_equal(preds$upper[1], 0)
})
