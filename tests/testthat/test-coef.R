test_that("coef dispatches and matches survival summaries by variable", {
    for (longitudinal in list(NULL, LongitudinalSteinFojo())) {
        fit <- make_coefficient_samples(
            c("beta_os_cov[2]", "lambda", "beta_os_cov[1]"),
            longitudinal,
            survival = TRUE
        )
        actual <- stats::coef(fit)
        expect_s3_class(actual, "tbl_df")
        expect_identical(actual$component, rep("survival", 2))
        expect_identical(actual$predictor, rep(NA_character_, 2))
        expect_identical(actual$covariate, c("age", "armB"))
        expect_identical(
            actual$variable, c("beta_os_cov[1]", "beta_os_cov[2]")
        )
        # Distinct known medians detect incorrectly attached covariate labels.
        expect_equal(actual$median, c(25.5, 5.5) / 30)
        expected <- posterior::summarise_draws(
            subset(fit@results$draws(), variable = actual$variable)
        )
        for (column in names(expected)) {
            expect_equal(actual[[column]], expected[[column]])
        }
    }
})

test_that("coef labels interactions and separates longitudinal predictors", {
    model <- LongitudinalSteinFojoCov(
        mu_b_formula = ~1, omega_b_formula = ~1,
        mu_g_formula = ~arm * age, omega_g_formula = ~age,
        mu_s_formula = ~arm, omega_s_formula = ~1
    )
    variables <- c(
        "beta_os_cov[1]", "beta_os_cov[2]",
        "lm_sfc_mu_g_coefficients[3]", "lm_sfc_mu_g_coefficients[1]",
        "lm_sfc_mu_g_coefficients[2]", "lm_sfc_mu_s_coefficients[1]",
        "lm_sfc_omega_g_coefficients[1]", "lm_sfc_mu_g_intercept",
        "link_growth"
    )
    actual <- coef(make_coefficient_samples(variables, model, survival = TRUE))
    expect_identical(
        actual$component, c(rep("survival", 2), rep("longitudinal", 5))
    )
    # Match by predictor and covariate, without assuming predictor order.
    labels <- paste(actual$predictor, actual$covariate, sep = ":")
    expect_setequal(labels, c(
        "NA:age", "NA:armB", "mu_g:armB", "mu_g:age", "mu_g:armB:age",
        "mu_s:armB", "omega_g:age"
    ))
    expect_identical(
        actual$variable[match("mu_g:armB:age", labels)],
        "lm_sfc_mu_g_coefficients[3]"
    )
    expect_identical(
        actual$covariate[actual$predictor %in% "mu_g"],
        c("armB", "age", "armB:age")
    )
    expect_equal(actual$median[match("mu_g:armB:age", labels)], 25.5 / 90)
})

test_that("coef supports every longitudinal covariate model family", {
    models <- list(
        LongitudinalGSFCov(), LongitudinalSteinFojoCov(),
        LongitudinalClaretBrunoCov(), LongitudinalRandomSlopeCov()
    )
    for (model in models) {
        fit <- make_coefficient_samples("placeholder", model)
        mapping <- covariates(model, data = fit@data@subject)
        variables <- unlist(Map(function(parameter, covariates) {
            if (length(covariates)) {
                paste0(parameter, "[", seq_along(covariates), "]")
            }
        }, names(mapping), mapping), use.names = FALSE)
        fit <- make_coefficient_samples(variables, model)
        actual <- coef(fit)
        expect_identical(actual$component, rep("longitudinal", length(variables)))
        expect_identical(
            actual$covariate,
            if (is(model, "LongitudinalRandomSlopeCov")) {
                c("studyY", "armB", "armB")
            } else {
                c("studyY", "studyY", rep("armB", length(variables) - 2L))
            }
        )
        expect_identical(actual$variable, variables)
        expect_false(anyNA(actual$predictor))
    }
})

test_that("coef forwards summary functions and quantile probabilities", {
    fit <- make_coefficient_samples(
        c("beta_os_cov[1]", "beta_os_cov[2]"), survival = TRUE
    )
    actual <- coef(fit, "median", intervals = function(x) {
        stats::quantile(x, probs = c(0.025, 0.975))
    })
    expect_true(all(c("2.5%", "97.5%") %in% names(actual)))
    expect_false(any(c("q5", "q95") %in% names(actual)))
    expect_equal(actual[["2.5%"]], c(1.225, 11.225) / 20)
    actual <- coef(fit, "mean", "sd")
    expect_identical(names(actual), c(
        "component", "predictor", "covariate", "variable", "mean", "sd"
    ))
})

test_that("coef reports absent coefficients and inconsistent summaries", {
    fit <- make_coefficient_samples("mu_kg", LongitudinalSteinFojo())
    expect_error(coef(fit), "No covariate coefficients")
    fit <- make_coefficient_samples(
        "lm_sfc_mu_g_intercept",
        LongitudinalSteinFojoCov(
            mu_b_formula = ~1, omega_b_formula = ~1,
            mu_g_formula = ~1, omega_g_formula = ~1,
            mu_s_formula = ~1, omega_s_formula = ~1
        )
    )
    expect_error(coef(fit), "No covariate coefficients")
    fit <- make_coefficient_samples("beta_os_cov[1]", survival = TRUE)
    # Simulate an incomplete summary independently of backend validation.
    fit@results$summary <- function(variables, ...) {
        tibble::tibble(variable = "beta_os_cov[1]", median = 0)
    }
    expect_error(coef(fit), "do not match the model design matrices")
})
