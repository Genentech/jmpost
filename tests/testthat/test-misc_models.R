test_data_1 <- ensure_test_data_1()

test_that("Longitudinal Model doesn't print sampler rejection messages", {
    # These rejections typically happen when the sampler samples a
    # 0 value for the variance parameter. Sensible initial values +
    # setting near 0 limits (as opposed to 0) should avoid this.

    mp <- capture_messages({
        devnull_out <- capture.output({
            devnull_model <- sampleStanModel(
                test_data_1$jmodel,
                data = test_data_1$jdata,
                iter_sampling = 3,
                iter_warmup = 3,
                chains = 1,
                refresh = 0,
                parallel_chains = 1,
                seed = 324
            )
        })
    })

    expect_false(any(grepl(
        "The current Metropolis proposal is about to be rejected",
        mp
    )))
})

test_that("Survival model can be fit with a horseshoe prior", {
    jm <- JointModel(
        survival = SurvivalWeibullPH(
            beta = prior_horseshoe(
                df = 1,
                df_global = 1,
                df_slab = 4,
                scale_global = 0.3,
                scale_slab = 2
            )
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = test_data_1$jdata,
            iter_sampling = 3,
            iter_warmup = 3,
            chains = 1,
            refresh = 0,
            parallel_chains = 1,
            seed = 325
        )
    })

    draws <- cmdstanr::as.CmdStanMCMC(mp)$draws()
    variables <- posterior::variables(draws)

    expect_true(all(
        c(
            "beta_os_cov[1]",
            "prior_local_beta_os_cov[1]",
            "prior_global_beta_os_cov",
            "prior_slab_beta_os_cov",
            "prior_shrinkage_factors_beta_os_cov[1]",
            "prior_shrinkage_factors_beta_os_cov[2]",
            "prior_shrinkage_factors_beta_os_cov[3]",
            "prior_scales_beta_os_cov[1]"
        ) %in%
            variables
    ))
})
