test_that("LongitudinalSteinFojoCov constructs predictors and model data", {
    model <- LongitudinalSteinFojoCov(
        mu_b_formula = ~study + age,
        omega_b_formula = ~study,
        mu_s_formula = ~arm,
        omega_s_formula = ~arm,
        mu_g_formula = ~arm,
        omega_g_formula = ~arm
    )

    expect_s4_class(model, "LongitudinalSteinFojoCov")
    expect_equal(model@mu_b_formula, ~study + age)
    expect_equal(model@omega_g_parametrization, "log-linear")
    expect_false(model@centred_baseline)
    expect_setequal(
        names(getParameters(model)),
        c(
            paste0("lm_sfc_", rep(c("mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g"), each = 2), c("_intercept", "_coefficients")),
            "lm_sfc_sigma",
            "lm_sfc_eta_tilde_b", "lm_sfc_eta_tilde_s", "lm_sfc_eta_tilde_g"
        )
    )

    subject <- DataSubject(
        data.frame(
            subject = paste0("S", 1:4),
            arm = c("A", "B", "A", "B"),
            study = c("X", "X", "Y", "Y"),
            age = c(50, 60, 55, 65)
        ),
        "subject", "arm", "study"
    )
    stan_data <- longitudinal_model_stan_data(model, subject)
    expect_equal(stan_data$p_lm_sfc_mu_b, 2)
    expect_equal(stan_data$p_lm_sfc_omega_b, 1)
    expect_equal(stan_data$p_lm_sfc_mu_s, 1)
    expect_equal(
        stan_data$lm_sfc_mu_b_design,
        cbind(studyY = c(0, 0, 1, 1), age = c(50, 60, 55, 65)),
        ignore_attr = TRUE
    )
    expect_match(as.character(JointModel(model)), "lm_sfc_ind_mu_b", fixed = TRUE)
    expect_stan_syntax(JointModel(model))
})

test_that("covariate Stein-Fojo supports a centred baseline", {
    model <- LongitudinalSteinFojoCov(centred_baseline = TRUE)
    stan_code <- as.character(JointModel(model))

    expect_true(model@centred_baseline)
    expect_setequal(
        names(getParameters(model)),
        c(
            paste0(
                "lm_sfc_",
                rep(
                    c("mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g"),
                    each = 2
                ),
                c("_intercept", "_coefficients")
            ),
            "lm_sfc_sigma", "lm_sfc_psi_b",
            "lm_sfc_eta_tilde_s", "lm_sfc_eta_tilde_g"
        )
    )
    expect_match(
        stan_code,
        "lm_sfc_psi_b ~ lognormal(lm_sfc_ind_mu_b, lm_sfc_ind_omega_b);",
        fixed = TRUE
    )
    expect_false(grepl("lm_sfc_eta_tilde_b", stan_code, fixed = TRUE))
    expect_true(all(initialValues(model, n_chains = 1)[[1]]$lm_sfc_psi_b > 0))
    expect_stan_syntax(JointModel(model))
})

test_that("covariate Stein-Fojo reduces to the original formulation", {
    subject <- DataSubject(
        data.frame(
            subject = paste0("S", 1:8),
            arm = factor(rep(c("A", "B"), each = 4)),
            study = factor(rep(c("X", "Y"), times = 4))
        ),
        "subject", "arm", "study"
    )
    old_model <- LongitudinalSteinFojo(
        mu_bsld = prior_const_vector(log(c(60, 75))),
        omega_bsld = prior_const_vector(c(0.2, 0.3)),
        mu_ks = prior_const_vector(log(c(0.5, 0.4))),
        omega_ks = prior_const_vector(c(0.2, 0.25)),
        mu_kg = prior_const_vector(log(c(0.3, 0.35))),
        omega_kg = prior_const_vector(c(0.15, 0.2))
    )
    cov_model <- LongitudinalSteinFojoCov(
        mu_b_intercept_prior = prior_const(log(60)),
        mu_b_coefficients_prior = prior_const_vector(log(75) - log(60)),
        omega_b_intercept_prior = prior_const(log(0.2)),
        omega_b_coefficients_prior = prior_const_vector(log(0.3 / 0.2)),
        mu_s_intercept_prior = prior_const(log(0.5)),
        mu_s_coefficients_prior = prior_const_vector(log(0.4) - log(0.5)),
        omega_s_intercept_prior = prior_const(log(0.2)),
        omega_s_coefficients_prior = prior_const_vector(log(0.25 / 0.2)),
        mu_g_intercept_prior = prior_const(log(0.3)),
        mu_g_coefficients_prior = prior_const_vector(log(0.35) - log(0.3)),
        omega_g_intercept_prior = prior_const(log(0.15)),
        omega_g_coefficients_prior = prior_const_vector(log(0.2 / 0.15))
    )
    old_parameters <- as_stan_list(getParameters(old_model))
    cov_parameters <- as_stan_list(getParameters(cov_model))
    old_indexes <- as_stan_list(subject)
    cov_data <- longitudinal_model_stan_data(cov_model, subject)

    expected <- list(
        mu_b = old_parameters$prior_const_lm_sf_mu_bsld[old_indexes$subject_study_index],
        omega_b = old_parameters$prior_const_lm_sf_omega_bsld[old_indexes$subject_study_index],
        mu_s = old_parameters$prior_const_lm_sf_mu_ks[old_indexes$subject_arm_index],
        omega_s = old_parameters$prior_const_lm_sf_omega_ks[old_indexes$subject_arm_index],
        mu_g = old_parameters$prior_const_lm_sf_mu_kg[old_indexes$subject_arm_index],
        omega_g = old_parameters$prior_const_lm_sf_omega_kg[old_indexes$subject_arm_index]
    )
    for (name in names(expected)) {
        actual <- .covariate_predictor_r(
            cov_data[[paste0("lm_sfc_", name, "_design")]],
            cov_parameters[[paste0("prior_const_lm_sfc_", name, "_intercept")]],
            cov_parameters[[paste0("prior_const_lm_sfc_", name, "_coefficients")]],
            slot(cov_model, paste0(name, "_parametrization"))
        )
        expect_equal(actual, expected[[name]])
    }
})

test_that("covariate Stein-Fojo supports links and generated quantities", {
    model <- LongitudinalSteinFojoCov()
    linked <- JointModel(model, SurvivalExponential(), link = linkDSLD())
    expect_match(as.character(linked), "lm_sfc_psi_b", fixed = TRUE)
    expect_match(as.character(linked), "link_dsld_contrib", fixed = TRUE)
    expect_equal(getPredictionNames(model), c("b", "s", "g"))
    expect_equal(
        getRandomEffectsNames(model),
        c(b = "lm_sfc_psi_b", s = "lm_sfc_psi_s", g = "lm_sfc_psi_g")
    )

    samples <- .JointModelSamples(
        model = JointModel(model),
        data = structure(1, class = "DataJoint"),
        results = structure(1, class = "CmdStanMCMC")
    )
    population_module <- as.StanModule(
        samples,
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
        "gq_lm_sfc_mu_b_design",
        fixed = TRUE
    )
    expect_stan_syntax(population_module)
})

test_that("SimLongitudinalSteinFojoCov uses covariate predictors", {
    sim <- SimLongitudinalSteinFojoCov(
        times = c(0, 1),
        mu_b_intercept = log(60),
        mu_b_coefficients = c(0, log(2)),
        omega_b_intercept = log(0.2),
        omega_b_coefficients = c(0, log(1.5)),
        mu_s_intercept = log(0.5),
        mu_s_coefficients = c(0, log(0.8) - log(0.5)),
        omega_s_intercept = log(0.1),
        omega_s_coefficients = c(0, log(2)),
        mu_g_intercept = log(0.3),
        mu_g_coefficients = c(0, log(0.4) - log(0.3)),
        omega_g_intercept = log(0.15),
        omega_g_coefficients = c(0, log(2))
    )
    subjects <- data.frame(
        subject = c("S1", "S2"),
        arm = factor(c("A", "B")),
        study = factor(c("X", "Y"))
    )

    set.seed(123)
    sampled <- sampleSubjects(sim, subjects)
    set.seed(123)
    expect_equal(sampled$psi_b, c(rlnorm(1, log(60), 0.2), rlnorm(1, log(120), 0.3)))
    expect_true(all(c("psi_s", "psi_g") %in% names(sampled)))
    observations <- sampleObservations(sim, transform(sampled, time = 0))
    expect_true(all(is.finite(observations$sld)))
})

test_that("posterior Stein-Fojo covariate draws create a matching simulator", {
    model <- LongitudinalSteinFojoCov()
    parameter_names <- c("mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g")
    values <- setNames(
        c(
            unlist(lapply(parameter_names, function(name) c(1, 0))),
            0.1
        ),
        c(
            unlist(lapply(parameter_names, function(name) c(
                paste0("lm_sfc_", name, "_intercept"),
                paste0("lm_sfc_", name, "_coefficients[1]")
            ))),
            "lm_sfc_sigma"
        )
    )
    simulator <- createLongitudinalSimObject(
        model,
        matrix(values, nrow = 1, dimnames = list(NULL, names(values)))
    )

    expect_s4_class(simulator, "SimLongitudinalSteinFojoCov")
    expect_equal(simulator@mu_b_formula, model@mu_b_formula)
    expect_equal(simulator@mu_s_coefficients, 0)
})
