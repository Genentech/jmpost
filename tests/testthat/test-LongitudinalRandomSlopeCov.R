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

test_that("old and covariate model parametrizations are consistent", {
    subjects <- DataSubject(
        data.frame(
            subject = paste0("S", 1:8),
            arm = factor(rep(c("A", "B"), each = 4)),
            study = factor(rep(c("X", "Y"), times = 4))
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
    )
    old_model <- LongitudinalRandomSlope(
        intercept = prior_const_vector(c(30, 45)),
        slope_mu = prior_const_vector(c(1, 2.5)),
        slope_sigma = prior_const_vector(c(0.4, 0.7)),
        sigma = prior_const(1.2)
    )
    cov_model <- LongitudinalRandomSlopeCov(
        mu_formula = ~study,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~arm,
        mu_intercept_prior = prior_const(30),
        mu_coefficients_prior = prior_const_vector(15),
        slope_mu_intercept_prior = prior_const(1),
        slope_mu_coefficients_prior = prior_const_vector(1.5),
        slope_sigma_intercept_prior = prior_const(log(0.4)),
        slope_sigma_coefficients_prior = prior_const_vector(log(0.7 / 0.4)),
        sigma = prior_const(1.2)
    )
    old_parameters <- as_stan_list(getParameters(old_model))
    cov_parameters <- as_stan_list(getParameters(cov_model))
    subject_indexes <- as_stan_list(subjects)
    cov_data <- longitudinal_model_stan_data(cov_model, subjects)

    old_intercept <- old_parameters$prior_const_lm_rs_intercept[
        subject_indexes$subject_study_index
    ]
    old_slope_mu <- old_parameters$prior_const_lm_rs_slope_mu[
        subject_indexes$subject_arm_index
    ]
    old_slope_sigma <- old_parameters$prior_const_lm_rs_slope_sigma[
        subject_indexes$subject_arm_index
    ]

    cov_intercept <- .covariate_predictor_r(
        cov_data$lm_rsc_mu_design,
        intercept = cov_parameters$prior_const_lm_rsc_mu_intercept,
        coefficients = cov_parameters$prior_const_lm_rsc_mu_coefficients,
        parametrization = cov_model@mu_parametrization
    )
    cov_slope_mu <- .covariate_predictor_r(
        cov_data$lm_rsc_slope_mu_design,
        intercept = cov_parameters$prior_const_lm_rsc_slope_mu_intercept,
        coefficients = cov_parameters$prior_const_lm_rsc_slope_mu_coefficients,
        parametrization = cov_model@slope_mu_parametrization
    )
    cov_slope_sigma <- .covariate_predictor_r(
        cov_data$lm_rsc_slope_sigma_design,
        intercept = cov_parameters$prior_const_lm_rsc_slope_sigma_intercept,
        coefficients = cov_parameters$prior_const_lm_rsc_slope_sigma_coefficients,
        parametrization = cov_model@slope_sigma_parametrization
    )

    expect_equal(cov_intercept, old_intercept)
    expect_equal(cov_slope_mu, old_slope_mu)
    expect_equal(cov_slope_sigma, old_slope_sigma)

    individual_slope <- seq(0.8, 2.2, length.out = 8)
    time <- seq(0, 3.5, length.out = 8)
    expect_equal(
        cov_intercept + individual_slope * time,
        old_intercept + individual_slope * time
    )
})

test_that("covariate formula helpers cover validation and edge cases", {
    expect_equal(
        .validate_covariate_formula(~arm, "formula"),
        ~arm
    )
    expect_equal(
        .validate_covariate_parametrization("linear", "parametrization"),
        "linear"
    )

    data <- data.frame(
        arm = factor(c("A", "B", "A")),
        study = factor(rep("X", 3)),
        age = c(50, 60, 70)
    )
    expect_equal(
        .covariate_design_matrix(~arm + age, data),
        cbind(armB = c(0, 1, 0), age = c(50, 60, 70)),
        ignore_attr = TRUE
    )
    expect_equal(dim(.covariate_design_matrix(~study, data)), c(3, 0))
    expect_error(
        .covariate_design_matrix(~missing, data),
        "missing"
    )
    data$age[[2]] <- NA_real_
    expect_error(.covariate_design_matrix(~age, data), "missing values")
})

test_that("covariate predictor helpers implement all parametrizations", {
    design <- matrix(c(0, 1), ncol = 1)
    expect_equal(.covariate_predictor_r(design, 2, 0.5, "linear"), c(2, 2.5))
    expect_equal(
        .covariate_predictor_r(design, 2, 0.5, "proportional"),
        c(2, 3)
    )
    expect_equal(
        .covariate_predictor_r(design, 2, log(3), "exponential"),
        c(2, 6)
    )
    expect_equal(
        .covariate_predictor_r(design, log(2), log(3), "log-linear"),
        c(2, 6)
    )

    expect_match(
        .covariate_predictor_stan("theta", "linear"),
        "rep_vector(theta_intercept",
        fixed = TRUE
    )
    expect_match(
        .covariate_predictor_stan("theta", "log-linear"),
        "exp(rep_vector(theta_intercept",
        fixed = TRUE
    )

    positive_prior <- .positive_intercept_prior(
        prior_normal(1, 1),
        "exponential"
    )
    expect_equal(
        positive_prior@limits[[1]],
        getOption("jmpost.double_eps")
    )
    expect_equal(
        .predictor_reference_value(prior_const(log(2)), "log-linear"),
        2
    )
    expect_identical(
        longitudinal_model_stan_data(LongitudinalRandomSlope(), NULL),
        list()
    )
})
