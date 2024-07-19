# LongitudinalGSF ----

test_that("LongitudinalGSF works as expected with default arguments", {
    result <- expect_silent(LongitudinalGSF())
    expect_s4_class(result, "LongitudinalGSF")
})



test_that("Print method for LongitudinalGSF works as expected", {

    expect_snapshot({
        x <- LongitudinalGSF()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalGSF(
            sigma = prior_normal(0, 1),
            mu_kg = prior_gamma(2, 1)
        )
        print(x)
    })
})


test_that("Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalGSF(centred = TRUE),
        survival = SurvivalWeibullPH(),
        link = Link(linkTTG(), linkDSLD(), linkGrowth())
    )
    expect_false(any(
        c("lm_gsf_eta_tilde_kg", "lm_gsf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_gsf_psi_kg", "lm_gsf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalGSF(centred = FALSE),
        survival = SurvivalLogLogistic(),
        link = linkDSLD()
    )
    expect_true(all(
        c("lm_gsf_eta_tilde_kg", "lm_gsf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_gsf_psi_kg", "lm_gsf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Can load and compile growth + shrinkage links", {
    jm <- JointModel(
        longitudinal = LongitudinalGSF(centred = FALSE),
        survival = SurvivalWeibullPH(),
        link = Link(linkShrinkage(), linkGrowth())
    )
    expect_true(all(
        c("link_growth", "link_shrinkage") %in% names(jm@parameters)
    ))
    expect_true(
        grepl("// Source - lm-gsf/link_shrinkage.stan", as.character(jm))
    )
    expect_true(
        grepl("// Source - lm-gsf/link_growth.stan", as.character(jm))
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Can recover known distributional parameters from a full GSF joint model", {

    skip_if_not(is_full_test())

    set.seed(7743)
    jlist <- SimJointData(
        design = list(
            SimGroup(120, "Arm-A", "Study-X"),
            SimGroup(140, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / (400 / 365),
            time_max = 3,
            time_step = 1 / 365,
            lambda_censor = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = -0.1,
                "C" = 0.5
            ),
            beta_cont = 0.3
        ),
        longitudinal = SimLongitudinalGSF(
            times = c(-100, -50, 0, 1, 10, 50, 100, 150, 250, 300, 400, 500, 600) / 365,
            sigma = 0.01,
            mu_s = log(c(0.6, 0.4)),
            mu_g = log(c(0.25, 0.35)),
            mu_b = log(60),
            mu_phi = qlogis(c(0.4, 0.6)),
            omega_b = 0.2,
            omega_s = 0.2,
            omega_g = 0.2,
            omega_phi = 0.2,
            link_dsld = 0.1,
            link_ttg = 0.2,
            link_identity = 0
        ),
        .silent = TRUE
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = jlist@survival,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = jlist@survival,
            formula = Surv(time, event) ~ cov_cat + cov_cont
        ),
        longitudinal = DataLongitudinal(
            data = jlist@longitudinal,
            formula = sld ~ time
        )
    )

    jm <- JointModel(
        longitudinal = LongitudinalGSF(
            mu_bsld = prior_normal(log(60), 0.4),
            mu_ks = prior_normal(log(0.6), 0.4),
            mu_kg = prior_normal(log(0.3), 0.4),
            mu_phi = prior_normal(qlogis(0.5), 0.5),
            omega_bsld = prior_lognormal(log(0.2), 0.4),
            omega_ks = prior_lognormal(log(0.2), 0.4),
            omega_kg = prior_lognormal(log(0.2), 0.4),
            omega_phi = prior_lognormal(log(0.2), 0.4),
            sigma = prior_lognormal(log(0.01), 0.4),
            centred = TRUE
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / (400 / 365)), 0.4)
        ),
        link = Link(
            linkDSLD(prior = prior_normal(0.1, 0.2)),
            linkTTG(prior = prior_normal(0.2, 0.2))
        )
    )

    suppressWarnings({
        mp <- run_quietly({
            sampleStanModel(
                jm,
                data = jdat,
                iter_warmup = 400,
                iter_sampling = 800,
                chains = 2,
                refresh = 200,
                parallel_chains = 2
            )
        })
    })

    summary_post <- function(model, vars, exp = FALSE) {
        no_name_quant <- \(...) {
            x <- quantile(...)
            names(x) <- NULL
            x
        }
        dat <- model$summary(
            vars,
            mean = mean,
            q01 = \(x) no_name_quant(x, 0.01),
            q99 = \(x) no_name_quant(x, 0.99),
            rhat = posterior::rhat,
            ess_bulk = posterior::ess_bulk,
            ess_tail = posterior::ess_tail
        )
        if (exp) {
            dat$q01 <- dat$q01 |> exp()
            dat$q99 <- dat$q99 |> exp()
            dat$mean <- dat$mean |> exp()
        }
        dat
    }

    dat <- summary_post(
        as.CmdStanMCMC(mp),
        c("lm_gsf_mu_bsld", "lm_gsf_mu_ks", "lm_gsf_mu_kg"),
        TRUE
    )

    true_values <- c(60, 0.6, 0.4, 0.25, 0.35)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))

    dat <- summary_post(
        as.CmdStanMCMC(mp),
        c("link_dsld", "link_ttg", "sm_exp_lambda", "lm_gsf_mu_phi")
    )

    true_values <- c(0.1, 0.2, 1 / (1 / (400 / 365)), qlogis(c(0.4, 0.6)))
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})



test_that("Quantity models pass the parser", {
    mock_samples <- .JointModelSamples(
        model = JointModel(longitudinal = LongitudinalGSF(centred = FALSE)),
        data = structure(1, class = "DataJoint"),
        results = structure(1, class = "CmdStanMCMC")
    )
    stanmod <- as.StanModule(
        mock_samples,
        generator = QuantityGeneratorPopulation(1, "A", "B"),
        type = "longitudinal"
    )
    expect_stan_syntax(stanmod)

    stanmod <- as.StanModule(
        mock_samples,
        generator = QuantityGeneratorSubject(1, "A"),
        type = "longitudinal"
    )
    expect_stan_syntax(stanmod)
})


test_that("Can generate valid initial values", {

    pars <- c(
        "lm_gsf_omega_bsld", "lm_gsf_omega_ks", "lm_gsf_omega_kg",
        "lm_gsf_a_phi", "lm_gsf_b_phi", "lm_gsf_sigma"
    )

    # Defaults work as expected
    mod <- LongitudinalGSF()
    vals <- initialValues(mod, n_chains = 1)
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))


    # Test all individual parameters throw error if given prior that can't sample
    # valid value
    args <- list(
        omega_bsld = prior_normal(-200, 1),
        omega_ks = prior_normal(-200, 1),
        omega_kg = prior_normal(-200, 1),
        omega_phi = prior_normal(-200, 1),
        sigma = prior_normal(-200, 1)
    )
    for (n_arg in names(args)) {
        arg <- args[n_arg]
        expect_error(
            {
                mod <- do.call(LongitudinalGSF, arg)
                initialValues(mod, n_chains = 1)
            },
            regexp = "Unable to generate"
        )
    }

    # Test initial values can be found for weird priors that do overlap the valid region
    mod <- LongitudinalGSF(
        omega_bsld = prior_normal(-200, 400),
        omega_ks = prior_gamma(2, 5),
        omega_kg = prior_uniform(-200, 400),
        omega_phi = prior_lognormal(-200, 2),
        sigma = prior_cauchy(-200, 400)
    )
    set.seed(1001)
    vals <- unlist(initialValues(mod, n_chains = 200))
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))

})
