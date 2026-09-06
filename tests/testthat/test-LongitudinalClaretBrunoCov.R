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
    values <- setNames(
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
