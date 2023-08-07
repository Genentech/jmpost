# LongitudinalGSF ----

test_that("LongitudinalGSF works as expected with default arguments", {
    result <- expect_silent(LongitudinalGSF())
    expect_s4_class(result, "LongitudinalGSF")
})

test_that("LongitudinalGSF works as expected with a single study", {
    set.seed(123)
    jlist <- suppressMessages(simulate_joint_data(
        times = 0:2000,
        lm_fun = sim_lm_random_slope(),
        os_fun = sim_os_weibull(
            lambda = 0.00333,
            gamma = 0.97
        )
    ))

    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(0, 50, 100, 150, 200, 250, 300)) |>
        dplyr::arrange(time, pt)

    jdat <- DataJoint(
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ cov_cat + cov_cont,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            subject = "pt",
            threshold = 5
        )
    )

    jm <- JointModel(
        link = LinkGSF(),
        longitudinal = LongitudinalGSF(),
        survival = SurvivalWeibullPH()
    )

    mp <- suppressWarnings(sampleStanModel(
        jm,
        data = jdat,
        iter_sampling = 100,
        iter_warmup = 100,
        chains = 1,
        parallel_chains = 1,
        exe_file = file.path(MODEL_DIR, "gsf_joint_model_with_link_single_study")
    ))

    expect_s4_class(mp, "JointModelSamples")
    expect_true(mp@results$summary("lm_gsf_mu_bsld")$ess_tail > 5)
})
