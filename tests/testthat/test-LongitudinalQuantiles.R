
test_that("Test that LongitudinalQuantities works as expected", {
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

    expected_column_names <- c("median", "lower", "upper", "time", "group")

    longsamps <- LongitudinalQuantities(
        mp,
        c("pt_00001", "pt_00002"),
        c(10, 20, 200, 300)
    )
    preds <- summary(longsamps)
    expect_equal(nrow(preds), 8)
    expect_equal(length(unique(preds$group)), 2)
    expect_equal(names(preds), expected_column_names)
    expect_equal(preds$type, NULL)


    longsamps <- LongitudinalQuantities(mp, time_grid = c(10, 20, 200, 300))
    preds <- summary(longsamps)
    expect_equal(nrow(preds), 4 * nrow(dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), dat_os$pt)


    longsamps <- LongitudinalQuantities(mp, c("pt_00001", "pt_00003"))
    preds <- preds <- summary(longsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)



    ### LongitudinalQuantities does not support group aggregation
    expect_error(
        LongitudinalQuantities(mp, groups = list("a" = c("pt_00011", "pt_00012"))),
        regexp = "groups is not a character vector"
    )


    ##### Section for autoplot

    samps <- LongitudinalQuantities(mp, c("pt_00011", "pt_00061"))
    p <- autoplot(
        samps,
        conf.level = FALSE
    )
    dat <- summary(samps)
    expect_length(p$layers, 2)
    expect_equal(p$labels$y, expression(y))
    expect_true(inherits(p$facet, "FacetWrap"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomPoint"))
    expect_equal(ggplot_build(p)$layout$layout |> nrow(), 2)


    samps <- LongitudinalQuantities(
        mp,
        groups = c("pt_00011", "pt_00061", "pt_00001", "pt_00002"),
        time_grid = c(10, 20, 50, 200)
    )
    p <- autoplot(samps)
    dat <- summary(samps)
    expect_length(p$layers, 3)
    expect_equal(p$labels$y, expression(y))
    expect_true(inherits(p$facet, "FacetWrap"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[3]]$geom, "GeomPoint"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomRibbon"))
    expect_equal(ggplot_build(p)$layout$layout |> nrow(), 4)
})





# The idea of this test is that we compare the medians of the generated quantities
# against the true values in the dataset
# As the model is perfect it should be an extremely high value meaning we are recovering
# the correct quantities
test_that("LongitudinalQuantities can recover known results", {
    set.seed(73339)
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

    longsamps <- LongitudinalQuantities(
        mp,
        time_grid = c(1, 100, 200, 250, 300, 350)
    )

    dat_sum <- dplyr::tibble(summary(longsamps)) |>
        dplyr::rename(pt = group)

    dat_all <- dat_lm |>
        dplyr::left_join(dat_sum, by = c("pt", "time")) |>
        dplyr::select(pt, time, sld, median) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(correl = cor(sld, median))

    expect_true(all(dat_all$correl > 0.99))
})
