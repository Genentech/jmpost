test_data_1 <- ensure_test_data_1()

test_that("coef agrees with summaries from a sampled CmdStanMCMC fit", {
    fit <- test_data_1$jsamples
    actual <- stats::coef(fit)
    expect_identical(actual$covariate, covariates(fit@data@survival))
    expected <- fit@results$summary(variables = actual$variable)
    for (column in names(expected)) {
        expect_equal(actual[[column]], expected[[column]])
    }
})

test_that("print works as expected for JointModelSamples", {
    expect_snapshot({
        print(test_data_1$jsamples)
    })
})


test_that("saving and restoring samples from disk works as expected", {
    samps <- test_data_1$jsamples

    tfile <- tempfile(fileext = ".Rds")
    saveObject(samps, file = tfile)

    samps2 <- readRDS(tfile)

    # Can't compare entire object as some components contain formulas
    # whose environment component will be different no matter what
    expect_equal(
        samps@data@survival@data,
        samps2@data@survival@data
    )
    expect_equal(
        samps@data@longitudinal@data,
        samps2@data@longitudinal@data
    )
    # Key bit is that the retieved samples are identical
    expect_equal(
        posterior::as_draws_df(samps@results),
        posterior::as_draws_df(samps2@results)
    )
})

test_that("survival shrinkage retains names in survival-only and joint fits", {
    variables <- c(
        "beta_os_cov[1]",
        "prior_shrinkage_factors_beta_os_cov[2]",
        "prior_shrinkage_factors_beta_os_cov[1]"
    )
    for (longitudinal in list(NULL, LongitudinalSteinFojo())) {
        fit <- make_shrinkage_samples(variables, longitudinal, survival = TRUE)
        actual <- shrinkage(fit)
        expected <- subset(fit@results$draws(), variable = variables[-1])
        posterior::variables(expected) <- c("armB", "age")
        expect_identical(actual, expected)
    }
})

test_that("joint shrinkage distinguishes longitudinal predictor covariates", {
    fit <- make_shrinkage_samples(
        c(
            "prior_shrinkage_factors_beta_os_cov[1]",
            "prior_shrinkage_factors_lm_sfc_mu_g_coefficients[2]",
            "prior_shrinkage_factors_lm_sfc_mu_g_coefficients[1]",
            "prior_shrinkage_factors_lm_sfc_mu_s_coefficients[1]",
            "prior_shrinkage_factors_lm_sfc_omega_g_coefficients[1]"
        ),
        LongitudinalSteinFojoCov(mu_g_formula = ~ arm * age),
        survival = TRUE
    )
    actual <- shrinkage(fit)
    expected <- fit@results$draws()
    posterior::variables(expected) <- c(
        "age",
        "longitudinal.mu_g:age",
        "longitudinal.mu_g:armB",
        "longitudinal.mu_s:armB",
        "longitudinal.omega_g:armB"
    )
    expect_identical(actual, expected)
})

test_that("longitudinal-only shrinkage labels all covariate model families", {
    models <- list(
        LongitudinalGSFCov(),
        LongitudinalSteinFojoCov(),
        LongitudinalClaretBrunoCov(),
        LongitudinalRandomSlopeCov()
    )
    parameters <- c(
        "lm_gsfc_mu_g_coefficients",
        "lm_sfc_mu_g_coefficients",
        "lm_clbrc_mu_g_coefficients",
        "lm_rsc_slope_mu_coefficients"
    )
    labels <- c(rep("longitudinal.mu_g:armB", 3), "longitudinal.slope_mu:armB")
    for (i in seq_along(models)) {
        fit <- make_shrinkage_samples(
            paste0("prior_shrinkage_factors_", parameters[i], "[1]"),
            models[[i]]
        )
        expected <- fit@results$draws()
        posterior::variables(expected) <- labels[i]
        expect_identical(shrinkage(fit), expected)
    }
})

test_that("unmapped scalar and vector factors retain their Stan names", {
    variables <- c(
        "prior_shrinkage_factors_custom[1]",
        "prior_shrinkage_factors_other",
        "unrelated"
    )
    fit <- make_shrinkage_samples(variables, LongitudinalSteinFojo())
    expect_identical(
        shrinkage(fit),
        subset(fit@results$draws(), variable = variables[1:2])
    )
})

test_that("shrinkage reports absent factors and invalid mapped indices", {
    fit <- make_shrinkage_samples("mu_kg", LongitudinalSteinFojo())
    expect_error(shrinkage(fit), "No sampled prior shrinkage factors")

    fit <- make_shrinkage_samples(
        "prior_shrinkage_factors_lm_sfc_mu_g_coefficients[2]",
        LongitudinalSteinFojoCov()
    )
    expect_error(shrinkage(fit), "do not match its design matrix")
})
