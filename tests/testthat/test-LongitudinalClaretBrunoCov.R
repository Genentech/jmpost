test_that("LongitudinalClaretBrunoCov constructs and renders", {
    model <- LongitudinalClaretBrunoCov(mu_p_formula = ~ arm + age)
    expect_s4_class(model, "LongitudinalClaretBrunoCov")
    expect_true(model@centred_baseline)
    expect_false(model@centred_inhibition)
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
    expect_equal(data$p_lm_clbrc_mu_p, 2)
    code <- as.character(JointModel(model))
    expect_match(code, "lm_clbrc_ind_mu_p", fixed = TRUE)
    expect_match(code, "ind_p_mod ./ ind_c", fixed = TRUE)
    expect_stan_syntax(code)
})

test_that("Claret-Bruno covariate model generates correctly named initial values", {
    initial_values <- initialValues(
        JointModel(longitudinal = LongitudinalClaretBrunoCov()),
        n_chains = 1
    )[[1]]

    expect_true(all(
        c(
            "lm_clbrc_psi_b",
            "lm_clbrc_eta_tilde_g",
            "lm_clbrc_eta_tilde_c",
            "lm_clbrc_eta_tilde_p"
        ) %in%
            names(initial_values)
    ))
    expect_false(any(
        c(
            "b.lm_clbrc_psi_b",
            "g.lm_clbrc_eta_tilde_g",
            "c.lm_clbrc_eta_tilde_c",
            "p.lm_clbrc_eta_tilde_p"
        ) %in%
            names(initial_values)
    ))
})

test_that("Claret-Bruno covariate model supports links and simulation", {
    model <- LongitudinalClaretBrunoCov()
    expect_equal(getPredictionNames(model), c("b", "g", "c", "p"))
    expect_equal(
        getRandomEffectsNames(model),
        c(
            b = "lm_clbrc_psi_b",
            g = "lm_clbrc_psi_g",
            c = "lm_clbrc_psi_c",
            p = "lm_clbrc_psi_p"
        )
    )
    expect_stan_syntax(JointModel(model, SurvivalExponential(), linkDSLD()))
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
        "gq_lm_clbrc_mu_p_design",
        fixed = TRUE
    )
    expect_match(
        as.character(population_module),
        "long_gq_pop_parameters[, 4]",
        fixed = TRUE
    )
    expect_stan_syntax(population_module)
    sim <- SimLongitudinalClaretBrunoCov()
    subjects <- data.frame(
        subject = c("S1", "S2"),
        arm = factor(c("A", "B")),
        study = factor(c("X", "Y"))
    )
    sampled <- sampleSubjects(sim, subjects)
    expect_true(all(
        sampled$psi_b > 0 &
            sampled$psi_g > 0 &
            sampled$psi_c > 0 &
            sampled$psi_p > 0
    ))
    observations <- sampleObservations(sim, transform(sampled, time = c(-1, 1)))
    expect_true(all(is.finite(observations$mu_sld)))
})

test_that("posterior Claret-Bruno covariate draws create a simulator", {
    model <- LongitudinalClaretBrunoCov()
    names <- c(
        "mu_b",
        "omega_b",
        "mu_g",
        "omega_g",
        "mu_c",
        "omega_c",
        "mu_p",
        "omega_p"
    )
    values <- stats::setNames(
        c(unlist(lapply(names, function(name) c(1, 0))), 0.1),
        c(
            unlist(lapply(names, function(name) {
                c(
                    paste0("lm_clbrc_", name, "_intercept"),
                    paste0("lm_clbrc_", name, "_coefficients[1]")
                )
            })),
            "lm_clbrc_sigma"
        )
    )
    simulator <- createLongitudinalSimObject(
        model,
        matrix(values, nrow = 1, dimnames = list(NULL, names(values)))
    )
    expect_s4_class(simulator, "SimLongitudinalClaretBrunoCov")
    expect_equal(simulator@mu_p_coefficients, 0)
})

test_that("Claret-Bruno covariate model recovers its parameters", {
    skip_if_not(is_full_test())

    predictor_truth <- c(
        mu_b_intercept = log(60),
        mu_b_coefficients = log(1.1),
        omega_b_intercept = log(0.12),
        omega_b_coefficients = log(1.2),
        mu_g_intercept = log(1),
        mu_g_coefficients = log(1.15),
        omega_g_intercept = log(0.2),
        omega_g_coefficients = log(1.2),
        mu_c_intercept = log(0.4),
        mu_c_coefficients = log(0.85),
        omega_c_intercept = log(0.15),
        omega_c_coefficients = log(1.2),
        mu_p_intercept = log(2),
        mu_p_coefficients = log(0.85),
        omega_p_intercept = log(0.2),
        omega_p_coefficients = log(1.2)
    )
    sigma <- 1
    set.seed(7044)
    parameter_names <- unique(sub(
        "_(intercept|coefficients)$",
        "",
        names(predictor_truth)
    ))
    formula_args <- stats::setNames(
        rep(list(~arm), length(parameter_names)),
        paste0(parameter_names, "_formula")
    )
    simulated <- SimJointData(
        design = list(
            SimGroup(175, "Arm-A", "Study-X"),
            SimGroup(175, "Arm-B", "Study-X")
        ),
        longitudinal = do.call(
            SimLongitudinalClaretBrunoCov,
            c(
                list(times = seq(0, 2.5, length.out = 16)),
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
        LongitudinalClaretBrunoCov,
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
        paste0("lm_clbrc_", names(truth)),
        format = "draws_matrix"
    )
    parameter <- sub("^lm_clbrc_", "", colnames(draws))
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
