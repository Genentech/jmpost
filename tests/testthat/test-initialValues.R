

test_that("initialValues() works as expected", {
    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalWeibullPH(),
        link = linkDSLD()
    )

    set.seed(341)
    initial_values <- initialValues(jm, n_chains = 2)

    # Ensure that we actually got 2 chains worth of initial values
    expect_length(initial_values, 2)

    # Ensure that we get different initial values per chain
    expect_true(
        initial_values[[1]]$lm_rs_intercept != initial_values[[2]]$lm_rs_intercept
    )

    # Ensure each inner list has the same parameters
    expect_equal(
        names(initial_values[[1]]),
        names(initial_values[[2]])
    )

    # Explicit test to ensure we got all the expected parameters
    expect_equal(
        c(
            "lm_rs_intercept", "lm_rs_slope_mu", "lm_rs_slope_sigma", "lm_rs_sigma",
            "lm_rs_ind_rnd_slope", "sm_weibull_ph_lambda",
            "sm_weibull_ph_gamma", "beta_os_cov", "link_dsld"
        ),
        names(initial_values[[1]])
    )


    # show that if we mock the random number generator, we get the same initial values
    ivs <- testthat::with_mocked_bindings(
        initialValues(jm, n_chains = 2),
        local_rnorm = \(...) 0,
        local_rbeta = \(...) 0,
        local_rlnorm = \(...) 0,
        local_rgamma = \(...) 0,
        local_runif = \(...) 0,
        local_rlogis = \(...) 0,
    )
    expect_equal(ivs[[1]], ivs[[2]])
})



test_that("ensure_initial_values() works as expected", {
    pars <- ParameterList(
        Parameter(name = "p1", prior = prior_beta(4, 2), size = 1),
        Parameter(name = "p2", prior = prior_normal(5, 1), size = "n_arms"),
        Parameter(name = "p3", prior = prior_lognormal(0, 1), size = 3)
    )

    ivs <- initialValues(pars, n_chains = 2)
    ivs[[1]]$p1 <- c(1)
    ivs[[1]]$p2 <- c(2, 3)
    ivs[[1]]$p3 <- c(4, 5, 6)

    dta <- list(n_arms = 2)

    res <- ensure_initial_values(ivs, dta, pars)

    # First check that the lengths have been expanded out to be the expected
    # sizes
    expect_length(res[[1]]$p1, 1)
    expect_length(res[[1]]$p2, 2)
    expect_length(res[[1]]$p3, 3)
    expect_length(res[[2]]$p1, 1)
    expect_length(res[[2]]$p2, 2)
    expect_length(res[[2]]$p3, 3)

    # Now double check that our pre-set values in the first element
    # have not been modified
    expect_equal(res[[1]]$p1, 1)
    expect_equal(res[[1]]$p2, array(c(2, 3), dim = c(2)))
    expect_equal(res[[1]]$p3, array(c(4, 5, 6), dim = c(3)))
})


test_that("intial values for fixed distributions gives valid values", {

    set.seed(3150)
    gsfmodel <- LongitudinalGSF(centred = TRUE)
    ivs <- initialValues(gsfmodel, n_chains = 100)

    for (values in ivs) {
        expect_true(plogis(values$lm_gsf_psi_phi_logit) > 0 & plogis(values$lm_gsf_psi_phi_logit) < 1)
        expect_true(values$lm_gsf_psi_bsld > 0)
        expect_true(values$lm_gsf_psi_ks > 0)
        expect_true(values$lm_gsf_psi_kg > 0)
    }
})
