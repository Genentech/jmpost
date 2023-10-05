
# Note that these are more just "smoke tests" e.g. we are looking for obvious signs
# that something has gone wrong. Given the dependence on complex objects generated
# by MCMC sampling this is hard to test deterministically.
# That being said all the individual components that comprise the function have been
# tested thoroughly so this is regarded as being sufficient
test_that("smoke test for summary(SurvivalQuantities) and autoplot(SurvivalQuantities)", {
    set.seed(739)
    jlist <- simulate_joint_data(
        n = c(250, 150),
        times = 1:2000,
        lambda_cen = 1 / 9000,
        lm_fun = sim_lm_random_slope(
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2,
            phi = 0
        ),
        os_fun = sim_os_exponential(1 / 100),
        .debug = TRUE,
        .silent = TRUE
    )

    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(0, 1, 100, 200, 250, 300, 350)) |>
        dplyr::arrange(pt, time)


    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / 100), 1 / 100)
        )
    )

    jdat <- DataJoint(
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ cov_cat + cov_cont,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            subject = "pt",
            threshold = 5
        )
    )
    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_sampling = 100,
        iter_warmup = 150,
        chains = 1,
        refresh = 0,
        parallel_chains = 1
    )

    expected_column_names <- c("median", "lower", "upper", "time", "group", "type")

    survsamps <- SurvivalQuantities(
        mp,
        list("a" = c("pt_00001", "pt_00002")),
        c(10, 20, 200, 300)
    )
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4)
    expect_equal(length(unique(preds$group)), 1)
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$type), "surv")


    survsamps <- SurvivalQuantities(mp, time_grid = c(10, 20, 200, 300))
    preds <- summary(survsamps)
    expect_equal(nrow(preds), 4 * nrow(dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), dat_os$pt)


    survsamps <- SurvivalQuantities(mp, c("pt_00001", "pt_00003"))
    preds <- preds <- summary(survsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)



    preds1 <- SurvivalQuantities(mp, "pt_00001", c(200, 300)) |> summary()
    preds2 <- SurvivalQuantities(mp, "pt_00001", c(200, 300), type = "cumhaz") |> summary()
    preds3 <- SurvivalQuantities(mp, "pt_00001", c(200, 300), type = "haz") |> summary()
    preds4 <- SurvivalQuantities(mp, "pt_00001", c(200, 300), type = "loghaz") |> summary()
    expect_equal(unique(preds1$type), "surv")
    expect_equal(unique(preds2$type), "cumhaz")
    expect_equal(unique(preds3$type), "haz")
    expect_equal(unique(preds4$type), "loghaz")
    expect_equal(round(preds1$median, 5), round(exp(-preds2$median), 5))
    expect_equal(round(preds3$median, 5), round(exp(preds4$median), 5))
    expect_true(all(preds1$median != preds3$median))


    ##### Section for autoplot,SurvivalSamples

    samps <- SurvivalQuantities(mp, c("pt_00011", "pt_00061"))
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
        mp,
        groups = list("a" = c("pt_00011", "pt_00061"), "b" = c("pt_00001", "pt_00002")),
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
        gtpt1 = sample(dat_os$pt, 20),
        gtpt2 = sample(dat_os$pt, 20),
        gtpt3 = sample(dat_os$pt, 20)
    )
    times <- seq(0, 100, by = 10)
    samps <- SurvivalQuantities(mp, ptgroups, times, type = "surv")
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
