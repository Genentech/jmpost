gq_population_stan_data.UserCovariateLongitudinalModel <- function(
    object,
    model,
    data = NULL,
    ...
) {
    list(
        declarations = "real gq_custom_population_design;",
        data = list(gq_custom_population_design = matrix(1, 1, 1))
    )
}


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

test_that("gq_population_stan_data() dispatches to user longitudinal models", {
    generator <- QuantityGeneratorPopulation(
        times = 1,
        studies = "X",
        arms = "A"
    )
    model <- structure(list(), class = "UserCovariateLongitudinalModel")

    result <- gq_population_stan_data(generator, model)

    expect_equal(result$declarations, "real gq_custom_population_design;")
    expect_equal(result$data$gq_custom_population_design, matrix(1, 1, 1))
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
        .covariate_predictor_r(design, 2, 2, "exponential"),
        c(1, 4)
    )
    expect_match(
        .covariate_predictor_stan("theta", "exponential"),
        "theta_intercept ^ (theta_design * theta_coefficients)",
        fixed = TRUE
    )
    expect_equal(
        .covariate_predictor_r(design, log(2), log(3), "log-linear"),
        c(2, 6)
    )
    expect_equal(
        .covariate_predictor_r(design, -1, 0.5, "logit-linear"),
        stats::plogis(c(-1, -0.5))
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
    expect_match(
        .covariate_predictor_stan("theta", "logit-linear"),
        "inv_logit(rep_vector(theta_intercept",
        fixed = TRUE
    )
    expect_match(
        .covariate_predictor_stan(
            "theta",
            "log-linear",
            design_prefix = "gq_theta",
            n_rows = "gq_n_quant"
        ),
        "rep_vector(theta_intercept, gq_n_quant) + gq_theta_design",
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
    expect_equal(
        .predictor_reference_value(prior_const(2), "exponential"),
        1
    )
    expect_identical(
        longitudinal_model_stan_data(LongitudinalRandomSlope(), NULL),
        list()
    )
})

test_that("covariate random-slope model exposes downstream parameter names", {
    model <- LongitudinalRandomSlopeCov()

    expect_equal(
        getRandomEffectsNames(model),
        c("slope" = "lm_rsc_ind_rnd_slope")
    )
    expect_equal(
        getPredictionNames(model),
        c("intercept", "slope")
    )
})

test_that("generated quantities rebuild model-aware Stan data", {
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2"),
            arm = c("A", "B"),
            study = c("X", "Y"),
            age = c(50, 60)
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
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
    model <- JointModel(LongitudinalRandomSlopeCov(
        mu_formula = ~study + age,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~arm
    ))
    samples <- .JointModelSamples(
        model = model,
        data = data,
        results = structure(list(), class = "CmdStanMCMC")
    )
    captured <- new.env(parent = emptyenv())
    compiled_model <- list(
        generate_quantities = function(data, fitted_params) {
            captured$data <- data
            "generated quantities"
        }
    )

    result <- testthat::with_mocked_bindings(
        generateQuantities(
            samples,
            generator = QuantityGeneratorSubject(0, "S1"),
            type = "longitudinal"
        ),
        compileStanModel = function(...) compiled_model,
        .package = "jmpost"
    )

    expect_equal(result, "generated quantities")
    expect_equal(captured$data$p_lm_rsc_mu, 2)
    expect_equal(captured$data$p_lm_rsc_slope_mu, 1)
    expect_equal(captured$data$p_lm_rsc_slope_sigma, 1)
    expect_equal(dim(captured$data$lm_rsc_mu_design), c(2, 2))
})

test_that("covariate random-slope quantity models pass the parser", {
    mock_samples <- .JointModelSamples(
        model = JointModel(longitudinal = LongitudinalRandomSlopeCov()),
        data = structure(1, class = "DataJoint"),
        results = structure(1, class = "CmdStanMCMC")
    )

    subject_module <- as.StanModule(
        mock_samples,
        generator = QuantityGeneratorSubject(1, "A"),
        type = "longitudinal"
    )
    expect_stan_syntax(subject_module)

    population_module <- as.StanModule(
        mock_samples,
        generator = QuantityGeneratorPopulation(
            times = 1,
            studies = "X",
            arms = "A",
            newdata = data.frame(study = "X", arm = "A")
        ),
        type = "longitudinal"
    )
    expect_match(
        as.character(population_module),
        "rep_vector(lm_rsc_mu_intercept, gq_n_quant)",
        fixed = TRUE
    )
    expect_stan_syntax(population_module)

    survival_samples <- .JointModelSamples(
        model = JointModel(
            longitudinal = LongitudinalRandomSlopeCov(),
            survival = SurvivalExponential(),
            link = linkDSLD()
        ),
        data = structure(1, class = "DataJoint"),
        results = structure(1, class = "CmdStanMCMC")
    )
    survival_module <- as.StanModule(
        survival_samples,
        generator = QuantityGeneratorPrediction(
            times = 1,
            newdata = data.frame(covariate = 0),
            params = list(intercept = 30, slope = 1)
        ),
        type = "survival"
    )
    expect_stan_syntax(survival_module)
})

test_that("population quantities accept all predictor covariates in newdata", {
    subject <- DataSubject(
        data.frame(
            subject = paste0("S", 1:4),
            arm = factor(c("A", "B", "A", "B")),
            study = factor(c("X", "X", "Y", "Y")),
            age = c(50, 60, 55, 65),
            sex = factor(c("F", "M", "F", "M")),
            variability_covariate = c(0.2, 0.3, 0.4, 0.5)
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
        mu_formula = ~study + age,
        slope_mu_formula = ~arm + sex,
        slope_sigma_formula = ~variability_covariate
    ))
    profiles <- data.frame(
        study = c("X", "Y"),
        arm = c("A", "B"),
        age = c(52, 62),
        sex = c("F", "M")
    )
    grid <- GridPopulation(
        times = c(0, 10),
        newdata = profiles
    )
    generator <- as.QuantityGenerator(grid, data, model = model)
    stan_data <- as_stan_list(generator, data = data, model = model)

    expect_equal(generator@times, c(0, 0, 10, 10))
    expect_equal(
        generator@newdata,
        profiles[c(1, 2, 1, 2), ],
        ignore_attr = TRUE
    )
    expect_equal(
        stan_data$gq_lm_rsc_mu_design,
        cbind(
            studyY = c(0, 1, 0, 1),
            age = c(52, 62, 52, 62)
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        stan_data$gq_lm_rsc_slope_mu_design,
        cbind(
            armB = c(0, 1, 0, 1),
            sexM = c(0, 1, 0, 1)
        ),
        ignore_attr = TRUE
    )
    expect_false("gq_lm_rsc_slope_sigma_design" %in% names(stan_data))

    collapser <- as.QuantityCollapser(grid, data, model = model)
    expect_equal(
        collapser@groups,
        rep(
            c(
                "study=X; arm=A; age=52; sex=F",
                "study=Y; arm=B; age=62; sex=M"
            ),
            2
        )
    )
})

test_that("population quantities require newdata for additional covariates", {
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2"),
            arm = c("A", "B"),
            study = c("X", "X"),
            age = c(50, 60)
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
    )
    data <- DataJoint(subject)
    model <- JointModel(LongitudinalRandomSlopeCov(mu_formula = ~study + age))

    expect_error(
        as.QuantityGenerator(GridPopulation(0), data, model = model),
        "newdata"
    )
    expect_error(
        as.QuantityGenerator(
            GridPopulation(
                0,
                newdata = data.frame(study = "X", arm = "A")
            ),
            data,
            model = model
        ),
        "age"
    )
})

test_that("required_longitudinal_covs() returns population predictor covariates", {
    random_slope <- LongitudinalRandomSlopeCov(
        mu_formula = ~study + age,
        slope_mu_formula = ~arm + sex,
        slope_sigma_formula = ~variability_covariate
    )
    stein_fojo <- LongitudinalSteinFojoCov(
        mu_b_formula = ~study + age,
        omega_b_formula = ~variability_covariate,
        mu_s_formula = ~arm + sex,
        omega_s_formula = ~variability_covariate,
        mu_g_formula = ~age,
        omega_g_formula = ~variability_covariate
    )

    expect_equal(
        required_longitudinal_covs(random_slope),
        c("study", "age", "arm", "sex")
    )
    expect_equal(
        required_longitudinal_covs(stein_fojo),
        c("study", "age", "arm", "sex")
    )
    expect_equal(required_longitudinal_covs(LongitudinalRandomSlope()), character())
})

test_that("required_simulation_covariates() includes variability predictors", {
    random_slope <- LongitudinalRandomSlopeCov(
        mu_formula = ~study + age,
        slope_mu_formula = ~arm + sex,
        slope_sigma_formula = ~variability_covariate
    )
    stein_fojo <- LongitudinalSteinFojoCov(
        mu_b_formula = ~study + age,
        omega_b_formula = ~variability_covariate,
        mu_s_formula = ~arm + sex,
        omega_s_formula = ~variability_covariate,
        mu_g_formula = ~age,
        omega_g_formula = ~variability_covariate
    )

    expect_equal(
        required_simulation_covariates(random_slope),
        c("study", "age", "arm", "sex", "variability_covariate")
    )
    expect_equal(
        required_simulation_covariates(stein_fojo),
        c("study", "age", "variability_covariate", "arm", "sex")
    )
    expect_equal(
        required_simulation_covariates(LongitudinalRandomSlope()),
        character()
    )
})

test_that("posterior random-slope covariate draws create a matching simulator", {
    model <- LongitudinalRandomSlopeCov()
    values <- c(
        lm_rsc_mu_intercept = 1,
        `lm_rsc_mu_coefficients[1]` = 0,
        lm_rsc_slope_mu_intercept = 0.1,
        `lm_rsc_slope_mu_coefficients[1]` = 0,
        lm_rsc_slope_sigma_intercept = -1,
        `lm_rsc_slope_sigma_coefficients[1]` = 0,
        lm_rsc_sigma = 0.2
    )

    simulator <- createLongitudinalSimObject(
        model,
        matrix(values, nrow = 1, dimnames = list(NULL, names(values)))
    )

    expect_s4_class(simulator, "SimLongitudinalRandomSlopeCov")
    expect_equal(simulator@mu_formula, model@mu_formula)
    expect_equal(simulator@slope_sigma_coefficients, 0)
})

test_that("posterior simulation preserves longitudinal covariates", {
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2"),
            arm = c("A", "A"),
            study = c("X", "X"),
            covariate = c(-1, 1),
            age = c(50, 60)
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
    )
    longitudinal <- SimLongitudinalRandomSlopeCov(
        times = c(0, 1),
        mu_formula = ~covariate,
        slope_mu_formula = ~covariate,
        slope_sigma_formula = ~covariate
    )

    survival <- SimSurvivalExponential(lambda = 1000, time_max = 1)
    survival@beta_os_cov <- 0
    result <- SimJointDataResults(
        subject = subject,
        surv_formula = survival::Surv(time, event) ~ age,
        longitudinal = rep(list(longitudinal), 2),
        survival = rep(list(survival), 2),
        .silent = TRUE
    )

    expect_s4_class(result, "SimJointData")
    expect_equal(nrow(result@longitudinal), 4)
})

test_that("population quantities infer study-arm profiles when sufficient", {
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2", "S3"),
            arm = c("A", "B", "B"),
            study = c("X", "X", "Y")
        ),
        subject = "subject",
        arm = "arm",
        study = "study"
    )
    data <- DataJoint(subject)
    model <- JointModel(LongitudinalRandomSlopeCov())

    generator <- as.QuantityGenerator(
        GridPopulation(times = 0),
        data,
        model = model
    )
    stan_data <- as_stan_list(generator, data = data, model = model)

    expect_equal(generator@arms, c("A", "B", "B"))
    expect_equal(generator@studies, c("X", "X", "Y"))
    expect_equal(dim(stan_data$gq_lm_rsc_mu_design), c(3, 1))
    expect_equal(dim(stan_data$gq_lm_rsc_slope_mu_design), c(3, 1))
})

test_that("LongitudinalRandomEffects extracts covariate random slopes", {
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
    draws <- function(variables, ...) {
        expect_equal(
            variables,
            c("lm_rsc_ind_rnd_slope[1]", "lm_rsc_ind_rnd_slope[2]")
        )
        matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
    }
    samples <- .JointModelSamples(
        model = JointModel(LongitudinalRandomSlopeCov()),
        data = DataJoint(subject),
        results = structure(list(draws = draws), class = "CmdStanMCMC")
    )

    result <- LongitudinalRandomEffects(samples)

    expect_equal(result@subject, c("S1", "S2"))
    expect_equal(result@parameter, c("slope", "slope"))
    expect_equal(dim(result@quantities), c(2, 2))
})
