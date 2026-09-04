test_that("LongitudinalGSFCov constructs all covariate predictors", {
    model <- LongitudinalGSFCov(mu_phi_formula = ~ arm + age)
    expect_s4_class(model, "LongitudinalGSFCov")
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
    sim <- SimLongitudinalGSFCov(
        mu_phi_coefficients = 0,
        omega_phi_coefficients = 0
    )
    subjects <- data.frame(
        subject = c("S1", "S2"),
        arm = factor(c("A", "B")),
        study = factor(c("X", "Y"))
    )
    sampled <- sampleSubjects(sim, subjects)
    expect_true(all(sampled$psi_phi > 0 & sampled$psi_phi < 1))
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
