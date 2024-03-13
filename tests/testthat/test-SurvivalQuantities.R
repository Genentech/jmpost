


test_that("SurvivalQuantities and autoplot.SurvivalQuantities works as expected", {
    ensure_test_data_1()

    expected_column_names <- c("median", "lower", "upper", "time", "group", "type")

    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        list("a" = c("pt_001", "pt_002")),
        c(10, 20, 200, 300)
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4)
    expect_equal(length(unique(preds$group)), 1)
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$type), "surv")


    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        time_grid = c(10, 20, 200, 300)
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4 * nrow(test_data_1$dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), test_data_1$dat_os$pt)


    survsamps <- SurvivalQuantities(
        test_data_1$jsamples,
        c("pt_001", "pt_003")
    )
    preds <- preds <- summary(survsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)


    # Check that the relationship between the quantitites is preservered e.g.
    # that `surv = exp(-cumhaz)`
    preds1 <- SurvivalQuantities(
        test_data_1$jsamples, "pt_001", c(200, 300)
    ) |> summary()
    preds2 <- SurvivalQuantities(
        test_data_1$jsamples, "pt_001", c(200, 300),
        type = "cumhaz"
    ) |> summary()
    preds3 <- SurvivalQuantities(
        test_data_1$jsamples, "pt_001", c(200, 300),
        type = "haz"
    ) |> summary()
    preds4 <- SurvivalQuantities(
        test_data_1$jsamples, "pt_001", c(200, 300),
        type = "loghaz"
    ) |> summary()
    expect_equal(unique(preds1$type), "surv")
    expect_equal(unique(preds2$type), "cumhaz")
    expect_equal(unique(preds3$type), "haz")
    expect_equal(unique(preds4$type), "loghaz")
    expect_equal(round(preds1$median, 5), round(exp(-preds2$median), 5))
    expect_equal(round(preds3$median, 5), round(exp(preds4$median), 5))
    expect_true(all(preds1$median != preds3$median))
})



test_that("autoplot.SurvivalSamples works as expected", {
    samps <- SurvivalQuantities(
        test_data_1$jsamples,
        c("pt_011", "pt_061")
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
        groups = list("a" = c("pt_011", "pt_061"), "b" = c("pt_001", "pt_002")),
        type = "loghaz",
        time_grid = c(10, 20, 50, 200)
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
    ptgroups <- list(
        gtpt1 = sample(test_data_1$dat_os$pt, 20),
        gtpt2 = sample(test_data_1$dat_os$pt, 20),
        gtpt3 = sample(test_data_1$dat_os$pt, 20)
    )
    times <- seq(0, 100, by = 10)
    samps <- SurvivalQuantities(test_data_1$jsamples, ptgroups, times, type = "surv")
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
        ptgroups <- list(
            gtpt1 = sample(test_data_1$dat_os$pt, 20),
            gtpt2 = sample(test_data_1$dat_os$pt, 20),
            gtpt3 = sample(test_data_1$dat_os$pt, 20)
        )
        times <- seq(0, 100, by = 10)
        samps_p1 <- SurvivalQuantities(
            test_data_1$jsamples,
            ptgroups,
            times,
            type = "surv"
        )
        print(samps_p1)
    })

    expect_snapshot({
        times <- seq(0, 100, by = 10)
        samps_p2 <- SurvivalQuantities(
            test_data_1$jsamples,
            time_grid = times,
            type = "loghaz"
        )
        print(samps_p2)
    })
})
