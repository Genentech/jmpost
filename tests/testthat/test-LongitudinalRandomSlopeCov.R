test_that("LongitudinalRandomSlopeCov constructs its parameters and Stan code", {
    model <- LongitudinalRandomSlopeCov(
        mu_formula = ~ study + age,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~arm,
        slope_sigma_parametrization = "log-linear"
    )

    expect_true(is(model, "LongitudinalRandomSlopeCov"))
    expect_equal(model@mu_formula, ~ study + age)
    expect_equal(model@slope_mu_parametrization, "linear")
    expect_equal(model@slope_sigma_parametrization, "log-linear")
    expect_setequal(
        names(getParameters(model)),
        c(
            "lm_rsc_mu_intercept",
            "lm_rsc_mu_coefficients",
            "lm_rsc_slope_mu_intercept",
            "lm_rsc_slope_mu_coefficients",
            "lm_rsc_slope_sigma_intercept",
            "lm_rsc_slope_sigma_coefficients",
            "lm_rsc_sigma",
            "lm_rsc_ind_rnd_slope"
        )
    )

    stan <- as.character(JointModel(model))
    expect_match(stan, "lm_rsc_mu_design", fixed = TRUE)
    expect_match(
        stan,
        "exp(rep_vector(lm_rsc_slope_sigma_intercept, n_subjects)",
        fixed = TRUE
    )

    linked_stan <- as.character(JointModel(
        model,
        SurvivalExponential(),
        link = linkDSLD()
    ))
    expect_match(linked_stan, "lm_rsc_ind_intercept", fixed = TRUE)
    expect_match(linked_stan, "link_dsld_contrib", fixed = TRUE)
})

test_that("model-aware as_stan_list creates subject covariate designs", {
    subject <- DataSubject(
        data.frame(
            subject = paste0("S", 1:4),
            arm = c("A", "B", "A", "B"),
            study = c("X", "X", "Y", "Y"),
            age = c(50, 60, 55, 65)
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
    )
    longitudinal <- DataLongitudinal(
        data.frame(
            subject = rep(paste0("S", 1:4), each = 2),
            time = rep(0:1, 4),
            value = seq_len(8)
        ),
        value ~ time
    )
    data <- DataJoint(subject, longitudinal = longitudinal)
    model <- JointModel(LongitudinalRandomSlopeCov(
        mu_formula = ~ study + age,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~1
    ))

    old_data <- as_stan_list(data)
    stan_data <- as_stan_list(data, model)

    expect_false("lm_rsc_mu_design" %in% names(old_data))
    expect_equal(stan_data$p_lm_rsc_mu, 2)
    expect_equal(stan_data$p_lm_rsc_slope_mu, 1)
    expect_equal(stan_data$p_lm_rsc_slope_sigma, 0)
    expect_equal(
        stan_data$lm_rsc_mu_design,
        cbind(studyY = c(0, 0, 1, 1), age = c(50, 60, 55, 65)),
        ignore_attr = TRUE
    )
    expect_equal(
        stan_data$lm_rsc_slope_mu_design,
        matrix(c(0, 1, 0, 1), ncol = 1),
        ignore_attr = TRUE
    )
})

test_that("model-aware as_stan_list handles one-level factors", {
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2"),
            arm = c("A", "B"),
            study = c("X", "X")
        ),
        "subject",
        "arm",
        "study"
    )
    longitudinal <- DataLongitudinal(
        data.frame(
            subject = rep(c("S1", "S2"), each = 2),
            time = rep(0:1, 2),
            value = 1:4
        ),
        value ~ time
    )
    data <- DataJoint(subject, longitudinal = longitudinal)
    model <- JointModel(LongitudinalRandomSlopeCov())

    stan_data <- as_stan_list(data, model)

    expect_equal(stan_data$p_lm_rsc_mu, 0)
    expect_equal(dim(stan_data$lm_rsc_mu_design), c(2, 0))
})

test_that("LongitudinalRandomSlopeCov validates formulas and parametrizations", {
    expect_error(
        LongitudinalRandomSlopeCov(mu_formula = value ~ study),
        "one-sided"
    )
    expect_error(
        LongitudinalRandomSlopeCov(mu_parametrization = "unknown"),
        "must be one of"
    )
})
