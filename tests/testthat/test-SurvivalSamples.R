
# Note that these are more just "smoke tests" e.g. we are looking for obvious signs
# that something has gone wrong. Given the dependence on complex objects generated
# by MCMC sampling this is hard to test deterministically.
# That being said all the individual components that comprise the function have been
# individually tested so this is regarded as being sufficient
test_that("smoke test for predict(SurvivalSamples) and autoplot(SurvivalSamples)", {
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

    survsamps <- SurvivalSamples(mp)


    ##### Section for predict,SurvivalSamples

    expected_column_names <- c("median", "lower", "upper", "time", "group", "type")

    preds <- predict(survsamps, list("a" = c("pt_00001", "pt_00002")), c(10, 20, 200, 300))
    expect_equal(nrow(preds), 4)
    expect_equal(length(unique(preds$group)), 1)
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$type), "surv")


    preds <- predict(survsamps, time_grid = c(10, 20, 200, 300))
    expect_equal(nrow(preds), 4 * nrow(dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), dat_os$pt)


    preds <- predict(survsamps, c("pt_00001", "pt_00003"))
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)


    preds1 <- predict(survsamps, "pt_00001", c(200, 300))
    preds2 <- predict(survsamps, "pt_00001", c(200, 300), type = "cumhaz")
    preds3 <- predict(survsamps, "pt_00001", c(200, 300), type = "haz")
    preds4 <- predict(survsamps, "pt_00001", c(200, 300), type = "loghaz")
    expect_equal(unique(preds1$type), "surv")
    expect_equal(unique(preds2$type), "cumhaz")
    expect_equal(unique(preds3$type), "haz")
    expect_equal(unique(preds4$type), "loghaz")
    expect_equal(round(preds1$median, 5), round(exp(-preds2$median), 5))
    expect_equal(round(preds3$median, 5), round(exp(preds4$median), 5))
    expect_true(all(preds1$median != preds3$median))


    ##### Section for autoplot,SurvivalSamples
    p <- autoplot(
        survsamps,
        add_wrap = FALSE,
        add_ci = FALSE,
        c("pt_00011", "pt_00061")
    )

    dat <- predict(survsamps, c("pt_00011", "pt_00061"))

    expect_length(p$layers, 1)
    expect_equal(p$labels$y, expression(S(t)))
    expect_true(inherits(p$facet, "FacetNull"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))

    p <- autoplot(
        survsamps,
        add_wrap = TRUE,
        patients = list("a" = c("pt_00011", "pt_00061"), "b" = c("pt_00001", "pt_00002")),
        time_grid = c(10, 20, 50, 200),
        type = "loghaz"
    )

    dat <- predict(
        survsamps,
        patients = list("a" = c("pt_00011", "pt_00061"), "b" = c("pt_00001", "pt_00002")),
        time_grid = c(10, 20, 50, 200),
        type = "loghaz"
    )

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

    p <- autoplot(
        survsamps,
        add_wrap = FALSE,
        add_ci = FALSE,
        add_km = TRUE,
        patients = ptgroups,
        time_grid = times,
        type = "surv"
    )

    dat <- predict(
        survsamps,
        patients = ptgroups,
        time_grid = times,
        type = "surv"
    )
    expect_length(p$layers, 3)
    expect_equal(p$labels$y, expression(S(t)))
    expect_true(inherits(p$facet, "FacetNull"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomKm"))
    expect_true(inherits(p$layers[[3]]$geom, "GeomKmTicks"))

})




test_that("summarise_by_group() works as expected", {
    get_ci_summary <- function(vec) {
        if (inherits(vec, "matrix")) {
            vec <- rowMeans(vec)
        }
        x <- data.frame(
            median = median(vec),
            lower = quantile(vec, 0.025),
            upper = quantile(vec, 0.975)
        )
        rownames(x) <- NULL
        x
    }

    x <- matrix(
        c(
            1, 10, 1, 3, 9,  23,
            2, 20, 1, 3, 10, 23,
            3, 30, 1, 3, 9,  23,
            4, 40, 2, 4, 12, 23,
            5, 50, 2, 4, 14, 23,
            6, 60, 2, 4, 10, 23
        ),
        byrow = TRUE,
        ncol = 6
    )
    colnames(x) <- sprintf(
        "quantity[%i,%i]",
    #     1  2  3  4  5  6    # Column index
        c(1, 1, 2, 2, 3, 3),  # Subject IDs
        c(4, 5, 4, 5, 4, 5)   # Time point IDs
    )
    draws_x <- posterior::as_draws_matrix(x)


    ## 1 subject 1 timepoint
    actual <- summarise_by_group(
        subject_index = 1,
        time_index = 4,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(x[, 1])
    )

    actual <- summarise_by_group(
        subject_index = 1,
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(x[, 2])
    )


    ## Select multiple subjects to collapse into a single "aggregate subject"
    ## at a single timepoint
    actual <- summarise_by_group(
        subject_index = c(1, 2),
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(rowMeans(x[, c(2, 4)]))
    )


    ## 1 subject at multiple time points
    actual <- summarise_by_group(
        subject_index = c(3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, 5]),
            get_ci_summary(x[, 6])
        )
    )


    ## Selecting multiple subjects to collapse into a single "agregate subject"
    ## at multiple timepoints
    actual <- summarise_by_group(
        subject_index = c(1, 3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, c(1, 5)]),
            get_ci_summary(x[, c(2, 6)])
        )
    )

    ## Can select the same subject multiple times
    actual <- summarise_by_group(
        subject_index = c(3, 3, 3, 2),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, c(5, 5, 5, 3)]),
            get_ci_summary(x[, c(6, 6, 6, 4)])
        )
    )
})
