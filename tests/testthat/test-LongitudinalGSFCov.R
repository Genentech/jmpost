test_that("LongitudinalGSFCov constructs all covariate predictors", {
    model <- LongitudinalGSFCov(mu_phi_formula = ~ arm + age)
    expect_s4_class(model, "LongitudinalGSFCov")
    expect_equal(model@mu_phi_parametrization, "linear")
    expect_true(model@centred_baseline)
    expect_false(model@centred_phi)
    expect_setequal(
        names(getParameters(model)),
        c(
            paste0(
                "lm_gsfc_",
                rep(
                    c(
                        "mu_b",
                        "omega_b",
                        "mu_s",
                        "omega_s",
                        "mu_g",
                        "omega_g",
                        "mu_phi",
                        "omega_phi"
                    ),
                    each = 2
                ),
                c("_intercept", "_coefficients")
            ),
            "lm_gsfc_sigma",
            "lm_gsfc_psi_b",
            "lm_gsfc_eta_tilde_s",
            "lm_gsfc_eta_tilde_g",
            "lm_gsfc_eta_tilde_phi"
        )
    )
    subject <- DataSubject(
        data.frame(
            subject = c("S1", "S2"),
            arm = c("A", "B"),
            study = c("X", "Y"),
            age = c(50, 60)
        ),
        "subject",
        "arm",
        "study"
    )
    data <- longitudinal_model_stan_data(model, subject)
    expect_equal(data$p_lm_gsfc_mu_phi, 2)
    expect_stan_syntax(JointModel(model))
    stan_code <- as.character(JointModel(model))
    expect_match(
        stan_code,
        "lm_gsfc_psi_phi_logit = lm_gsfc_ind_mu_phi +",
        fixed = TRUE
    )
    expect_false(grepl("logit(lm_gsfc_ind_mu_phi)", stan_code, fixed = TRUE))
})

test_that("GSF covariate model generates correctly named initial values", {
    initial_values <- initialValues(
        JointModel(longitudinal = LongitudinalGSFCov()),
        n_chains = 1
    )[[1]]

    expect_true(all(
        c(
            "lm_gsfc_psi_b",
            "lm_gsfc_eta_tilde_s",
            "lm_gsfc_eta_tilde_g",
            "lm_gsfc_eta_tilde_phi"
        ) %in%
            names(initial_values)
    ))
    expect_false(any(
        c(
            "b.lm_gsfc_psi_b",
            "s.lm_gsfc_eta_tilde_s",
            "g.lm_gsfc_eta_tilde_g",
            "phi.lm_gsfc_eta_tilde_phi"
        ) %in%
            names(initial_values)
    ))
})

test_that("LongitudinalGSFCov keeps phi within its positive epsilon bounds", {
    old_options <- options(jmpost.double_eps = 1e-10)
    on.exit(options(old_options), add = TRUE)

    stan_code <- as.character(JointModel(LongitudinalGSFCov()))
    expect_match(
        stan_code,
        "lm_gsfc_psi_phi = safe_inv_logit(lm_gsfc_psi_phi_logit, 1e-10);",
        fixed = TRUE
    )
    expect_stan_syntax(stan_code)
})

test_that("centred GSF covariate phi uses the logit-scale location directly", {
    stan_code <- as.character(JointModel(LongitudinalGSFCov(
        centred_phi = TRUE
    )))
    expect_match(
        stan_code,
        paste0(
            "lm_gsfc_psi_phi_logit ~ normal(lm_gsfc_ind_mu_phi, ",
            "lm_gsfc_ind_omega_phi);"
        ),
        fixed = TRUE
    )
    expect_false(grepl("logit(lm_gsfc_ind_mu_phi)", stan_code, fixed = TRUE))
    expect_stan_syntax(stan_code)
})

test_that("LongitudinalGSFCov supports links, quantities, and simulation", {
    model <- LongitudinalGSFCov()
    linked <- JointModel(model, SurvivalExponential(), linkDSLD())
    expect_match(as.character(linked), "lm_gsfc_psi_phi", fixed = TRUE)
    expect_equal(getPredictionNames(model), c("b", "s", "g", "phi"))
    expect_equal(
        getRandomEffectsNames(model),
        c(
            b = "lm_gsfc_psi_b",
            s = "lm_gsfc_psi_s",
            g = "lm_gsfc_psi_g",
            phi = "lm_gsfc_psi_phi"
        )
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
        "long_gq_pop_parameters[, 4] = inv_logit(",
        fixed = TRUE
    )
    expect_stan_syntax(population_module)
    sim <- SimLongitudinalGSFCov(
        mu_phi_intercept = qlogis(0.8),
        mu_phi_coefficients = 0,
        omega_phi_intercept = log(1e-10),
        omega_phi_coefficients = 0
    )
    subjects <- data.frame(
        subject = c("S1", "S2"),
        arm = factor(c("A", "B")),
        study = factor(c("X", "Y"))
    )
    sampled <- sampleSubjects(sim, subjects)
    expect_true(all(sampled$psi_phi > 0 & sampled$psi_phi < 1))
    expect_equal(sampled$psi_phi, rep(0.8, 2), tolerance = 1e-8)
    expect_true(all(is.finite(
        sampleObservations(sim, transform(sampled, time = 0))$sld
    )))
})

test_that("posterior GSF covariate draws create a matching simulator", {
    model <- LongitudinalGSFCov()
    names <- c(
        "mu_b",
        "omega_b",
        "mu_s",
        "omega_s",
        "mu_g",
        "omega_g",
        "mu_phi",
        "omega_phi"
    )
    values <- setNames(
        c(unlist(lapply(names, function(name) c(1, 0))), 0.1),
        c(
            unlist(lapply(names, function(name) {
                c(
                    paste0("lm_gsfc_", name, "_intercept"),
                    paste0("lm_gsfc_", name, "_coefficients[1]")
                )
            })),
            "lm_gsfc_sigma"
        )
    )
    simulator <- createLongitudinalSimObject(
        model,
        matrix(values, nrow = 1, dimnames = list(NULL, names(values)))
    )
    expect_s4_class(simulator, "SimLongitudinalGSFCov")
    expect_equal(simulator@mu_phi_coefficients, 0)
})

test_that("GSF covariate model recovers its parameters", {
    skip_if_not(is_full_test())

    predictor_truth <- c(
        mu_b_intercept = log(60),
        mu_b_coefficients = log(1.1),
        omega_b_intercept = log(0.2),
        omega_b_coefficients = log(1.2),
        mu_s_intercept = log(0.55),
        mu_s_coefficients = log(0.85),
        omega_s_intercept = log(0.2),
        omega_s_coefficients = log(1.2),
        mu_g_intercept = log(0.25),
        mu_g_coefficients = log(1.2),
        omega_g_intercept = log(0.18),
        omega_g_coefficients = log(1.2),
        mu_phi_intercept = qlogis(0.4),
        mu_phi_coefficients = 0.35,
        omega_phi_intercept = log(0.12),
        omega_phi_coefficients = log(1.2)
    )
    sigma <- 1
    set.seed(7043)
    parameter_names <- unique(sub(
        "_(intercept|coefficients)$",
        "",
        names(predictor_truth)
    ))
    formula_args <- setNames(
        rep(list(~arm), length(parameter_names)),
        paste0(parameter_names, "_formula")
    )
    simulated <- SimJointData(
        design = list(
            SimGroup(175, "Arm-A", "Study-X"),
            SimGroup(175, "Arm-B", "Study-X")
        ),
        longitudinal = do.call(
            SimLongitudinalGSFCov,
            c(
                list(times = seq(-0.25, 2.5, length.out = 16)),
                formula_args,
                as.list(predictor_truth),
                list(sigma = sigma, scaled_variance = FALSE)
            )
        ),
        survival = SimSurvivalExponential(0.1, time_max = 3, time_step = 1),
        .silent = TRUE
    )
    data <- DataJoint(
        subject = DataSubject(simulated@survival, "subject", "arm", "study"),
        longitudinal = DataLongitudinal(simulated@longitudinal, sld ~ time)
    )
    prior_args <- Map(
        function(value, name) {
            prior_normal(value, if (grepl("intercept$", name)) 0.5 else 0.4)
        },
        predictor_truth,
        names(predictor_truth)
    )
    names(prior_args) <- paste0(names(predictor_truth), "_prior")
    longitudinal <- do.call(
        LongitudinalGSFCov,
        c(
            formula_args,
            prior_args,
            list(
                sigma = prior_lognormal(log(sigma), 0.5),
                scaled_variance = FALSE
            )
        )
    )
    fit <- run_quietly(sampleStanModel(
        JointModel(longitudinal = longitudinal, link = Link()),
        data = data,
        iter_warmup = 1000,
        iter_sampling = 1500,
        chains = 2,
        parallel_chains = 2,
        refresh = 0
    ))
    truth <- c(predictor_truth, sigma = sigma)
    draws <- cmdstanr::as.CmdStanMCMC(fit)$draws(
        paste0("lm_gsfc_", names(truth)),
        format = "draws_matrix"
    )
    parameter <- sub("^lm_gsfc_", "", colnames(draws))
    parameter <- sub("\\[1\\]$", "", parameter)
    recovery <- data.frame(
        parameter = parameter,
        truth = unname(truth[parameter]),
        estimate = colMeans(draws),
        posterior_sd = apply(draws, 2, sd)
    )
    recovery$z_score <- with(
        recovery,
        (estimate - truth) / posterior_sd
    )
    expect_true(
        max(abs(recovery$z_score)) < 4,
        info = paste(capture.output(print(recovery)), collapse = "\n")
    )
})
