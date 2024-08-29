
test_that("LongitudinalSteinFojo works as expected with default arguments", {
    result <- expect_silent(LongitudinalSteinFojo())
    expect_s4_class(result, "LongitudinalSteinFojo")
})



test_that("Print method for LongitudinalSteinFojo works as expected", {

    expect_snapshot({
        x <- LongitudinalSteinFojo()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalSteinFojo(
            sigma = prior_normal(0, 1),
            mu_kg = prior_gamma(2, 1)
        )
        print(x)
    })
})


test_that("Centralised parameterisation compiles without issues", {
    jm <- JointModel(longitudinal = LongitudinalSteinFojo(centred = TRUE))
    expect_false(any(
        c("lm_sf_eta_tilde_kg", "lm_sf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_sf_psi_kg", "lm_sf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(as.character(x))
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(longitudinal = LongitudinalSteinFojo(centred = FALSE))
    expect_true(all(
        c("lm_sf_eta_tilde_kg", "lm_sf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_sf_psi_kg", "lm_sf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(as.character(x))
})



test_that("Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(centred = TRUE),
        survival = SurvivalExponential(),
        link = Link(linkTTG(), linkDSLD(), linkGrowth())
    )
    expect_false(any(
        c("lm_sf_eta_tilde_kg", "lm_sf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_sf_psi_kg", "lm_sf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(centred = FALSE),
        survival = SurvivalWeibullPH(),
        link = Link()
    )
    expect_true(all(
        c("lm_sf_eta_tilde_kg", "lm_sf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_sf_psi_kg", "lm_sf_psi_bsld") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Can load and compile growth + shrinkage links", {
    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(centred = FALSE),
        survival = SurvivalWeibullPH(),
        link = Link(linkShrinkage(), linkGrowth())
    )
    expect_true(all(
        c("link_growth", "link_shrinkage") %in% names(jm@parameters)
    ))
    expect_true(
        grepl("// Source - lm-stein-fojo/link_shrinkage.stan", as.character(jm))
    )
    expect_true(
        grepl("// Source - lm-stein-fojo/link_growth.stan", as.character(jm))
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})



test_that("Can recover known distributional parameters from a SF joint model", {

    skip_if_not(is_full_test())
    pars <- list(
        sigma = 0.01,
        mu_s = log(c(0.6, 0.4)),
        mu_g = log(c(0.25, 0.35)),
        mu_b = log(60),
        omega_b = c(0.2),
        omega_s = c(0.3, 0.1),
        omega_g = c(0.1, 0.3),
        omega_phi = c(0.3, 0.1),
        link_dsld = 0.1,
        link_ttg = 0.2,
        link_identity = 0,
        beta_cat_B = 0.5,
        beta_cat_C = -0.1,
        beta_cont = 0.3,
        lambda = 1 / (400 / 365)
    )
    set.seed(9898)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(180, "Arm-A", "Study-X"),
            SimGroup(190, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalSteinFojo(
            times = c(-100, -50, -10, 1, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900) * (1 / 365),
            sigma = pars$sigma,
            mu_s = pars$mu_s,
            mu_g = pars$mu_g,
            mu_b = pars$mu_b,
            omega_b = pars$omega_b,
            omega_s = pars$omega_s,
            omega_g = pars$omega_g,
            link_ttg = pars$link_ttg,
            link_dsld = pars$link_dsld,
            link_identity = pars$link_identity
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            time_step = 1 / 365,
            lambda = pars$lambda,
            lambda_cen = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = pars$beta_cat_B,
                "C" = pars$beta_cat_C
            ),
            beta_cont = pars$beta_cont
        ),
        .silent = TRUE
    )

    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(

            mu_bsld = prior_normal(log(60), 0.5),
            mu_ks = prior_normal(log(0.2), 0.5),
            mu_kg = prior_normal(log(0.2), 0.5),

            omega_bsld = prior_lognormal(log(0.1), 0.5),
            omega_ks = prior_lognormal(log(0.1), 0.5),
            omega_kg = prior_lognormal(log(0.1), 0.5),

            sigma = prior_lognormal(log(0.005), 0.5),
            centred = TRUE

        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(365 * (1 / 400)), 0.5)
        ),
        link = Link(
            linkTTG(prior_normal(-0.2, 0.5)),
            linkDSLD(prior_normal(0.2, 0.5))
        )
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
            formula = sld ~ time,
            threshold = 5
        )
    )

    ## Sample from JointModel

    set.seed(2213)

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 600,
            iter_warmup = 1000,
            chains = 2,
            parallel_chains = 2
        )
    })

    summary_post <- function(model, vars, exp = FALSE) {
        dat <- model$summary(
            vars,
            mean = mean,
            q01 = \(x) purrr::set_names(quantile(x, 0.01), ""),
            q99 = \(x) purrr::set_names(quantile(x, 0.99), ""),
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
        cmdstanr::as.CmdStanMCMC(mp),
        c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg")
    )
    true_values <- c(pars$mu_b, pars$mu_s, pars$mu_g)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))


    dat <- summary_post(
        cmdstanr::as.CmdStanMCMC(mp),
        c("lm_sf_sigma", "lm_sf_omega_bsld", "lm_sf_omega_kg", "lm_sf_omega_ks")
    )
    true_values <- c(pars$sigma, pars$omega_b, pars$omega_g, pars$omega_s)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))


    dat <- summary_post(
        cmdstanr::as.CmdStanMCMC(mp),
        c("link_dsld", "link_ttg", "sm_exp_lambda", "beta_os_cov")
    )
    true_values <- c(pars$link_dsld, pars$link_ttg, pars$lambda, pars$beta_cat_B, pars$beta_cat_C, pars$beta_cont)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})






test_that("Can recover known distributional parameters from a SF joint model with growth link", {

    skip_if_not(is_full_test())

    sim_params <- list(
        sigma = 0.005,
        mu_s = log(c(0.15, 0.3)),
        mu_g = log(c(0.4, 0.25)),
        mu_b = log(60),
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.2,
        link_ttg = 0,
        link_dsld = 0,
        link_growth = 1,
        lambda = 2,
        lambda_cen = 1 / 9000,
        beta_cat_b = -0.1,
        beta_cat_c = 0.5,
        beta_cont = 0.3
    )

    set.seed(2338)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(200, "Arm-A", "Study-X"),
            SimGroup(200, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalSteinFojo(
            times = c(
                -100, -50, -10, 1, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900,
                1100, 1300, 1500, 1800
            ) / 365,
            sigma = sim_params$sigma,
            mu_s = sim_params$mu_s,
            mu_g = sim_params$mu_g,
            mu_b = sim_params$mu_b,
            omega_b = sim_params$omega_b,
            omega_s = sim_params$omega_s,
            omega_g = sim_params$omega_g,
            link_ttg = sim_params$link_ttg,
            link_dsld = sim_params$link_dsld,
            link_growth = sim_params$link_growth
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            time_step = 1 / 365,
            lambda = sim_params$lambda,
            lambda_cen = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = sim_params$beta_cat_b,
                "C" = sim_params$beta_cat_c
            ),
            beta_cont = sim_params$beta_cont
        ),
        .silent = TRUE
    )

    # nolint start⁠
    ### Diagnostics helpers
    # plot(survival::survfit(Surv(time, event) ~ 1, data = jlist@survival))
    # median(jlist@survival$time)
    # nolint end


    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(

            mu_bsld = prior_normal(log(60), 0.5),
            mu_ks = prior_normal(log(0.2), 0.5),
            mu_kg = prior_normal(log(0.3), 0.5),

            omega_bsld = prior_lognormal(log(0.1), 0.5),
            omega_ks = prior_lognormal(log(0.1), 0.5),
            omega_kg = prior_lognormal(log(0.1), 0.5),

            sigma = prior_lognormal(log(0.005), 0.5),
            centred = TRUE

        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(2), 0.5)
        ),
        link = Link(
            linkGrowth(prior_normal(0, 2))
        )
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
            formula = sld ~ time,
            threshold = 5
        )
    )

    ## Sample from JointModel

    set.seed(2213)

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_warmup = 1500,
            iter_sampling = 1000,
            chains = 2,
            parallel_chains = 2
        )
    })

    summary_post <- function(model, vars, exp = FALSE) {
        dat <- model$summary(
            vars,
            mean = mean,
            q01 = \(x) purrr::set_names(quantile(x, 0.01), ""),
            q99 = \(x) purrr::set_names(quantile(x, 0.99), ""),
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
        cmdstanr::as.CmdStanMCMC(mp),
        c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg"),
        TRUE
    )
    true_values <- exp(c(sim_params$mu_b, sim_params$mu_s, sim_params$mu_g))
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))

    dat <- summary_post(
        cmdstanr::as.CmdStanMCMC(mp),
        c("link_growth", "sm_exp_lambda", "beta_os_cov")
    )
    true_values <- c(
        sim_params$link_growth, sim_params$lambda,
        sim_params$beta_cat_b, sim_params$beta_cat_c, sim_params$beta_cont
    )
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})






test_that("Quantity models pass the parser", {
    mock_samples <- .JointModelSamples(
        model = JointModel(longitudinal = LongitudinalSteinFojo(centred = TRUE)),
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
        "lm_gsf_sigma"
    )

    # Defaults work as expected
    mod <- LongitudinalSteinFojo()
    vals <- initialValues(mod, n_chains = 1)
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))


    # Test all individual parameters throw error if given prior that can't sample
    # valid value
    args <- list(
        omega_bsld = prior_normal(-200, 1),
        omega_ks = prior_normal(-200, 1),
        omega_kg = prior_normal(-200, 1),
        sigma = prior_normal(-200, 1)
    )
    for (n_arg in names(args)) {
        arg <- args[n_arg]
        expect_error(
            {
                mod <- do.call(LongitudinalSteinFojo, arg)
                initialValues(mod, n_chains = 1)
            },
            regexp = "Unable to generate"
        )
    }

    # Test initial values can be found for weird priors that do overlap the valid region
    mod <- LongitudinalSteinFojo(
        omega_bsld = prior_normal(-200, 400),
        omega_ks = prior_gamma(2, 5),
        omega_kg = prior_uniform(-200, 400),
        sigma = prior_cauchy(-200, 400)
    )
    set.seed(1001)
    vals <- unlist(initialValues(mod, n_chains = 200))
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))

})






test_that("Unscaled variance SF mode pass the parser", {
    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(centred = FALSE, scaled_variance = FALSE),
        survival = SurvivalLogLogistic(),
        link = linkDSLD()
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})






test_that("Can recover known distributional parameters from unscaled variance SF model", {

    skip_if_not(is_full_test())

    sim_params <- list(
        sigma = 0.4,
        mu_s = log(c(0.15, 0.3)),
        mu_g = log(c(0.4, 0.25)),
        mu_b = log(60),
        omega_b = 0.1,
        omega_s = c(0.1, 0.1),
        omega_g = c(0.2, 0.2),
        link_ttg = 0,
        link_dsld = 0,
        link_growth = 0,
        lambda = 2,
        lambda_cen = 1 / 9000,
        beta_cat_b = -0.1,
        beta_cat_c = 0.5,
        beta_cont = 0.3
    )

    set.seed(2338)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(200, "Arm-A", "Study-X"),
            SimGroup(200, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalSteinFojo(
            times = c(1, 50, 100, 150, 200, 300, 400, 500, 600, 700) / 365,
            sigma = sim_params$sigma,
            mu_s = sim_params$mu_s,
            mu_g = sim_params$mu_g,
            mu_b = sim_params$mu_b,
            omega_b = sim_params$omega_b,
            omega_s = sim_params$omega_s,
            omega_g = sim_params$omega_g,
            link_ttg = sim_params$link_ttg,
            link_dsld = sim_params$link_dsld,
            link_growth = sim_params$link_growth,
            scaled_variance = FALSE
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            time_step = 1 / 365,
            lambda = sim_params$lambda,
            lambda_cen = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = sim_params$beta_cat_b,
                "C" = sim_params$beta_cat_c
            ),
            beta_cont = sim_params$beta_cont
        ),
        .silent = TRUE
    )

    # nolint start⁠
    ### Diagnostics helpers
    # plot(survival::survfit(Surv(time, event) ~ 1, data = jlist@survival))
    # median(jlist@survival$time)
    # nolint end


    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(
            mu_bsld = prior_normal(log(60), 0.5),
            mu_ks = prior_normal(log(0.2), 0.5),
            mu_kg = prior_normal(log(0.3), 0.5),
            omega_bsld = prior_lognormal(log(0.1), 0.5),
            omega_ks = prior_lognormal(log(0.1), 0.5),
            omega_kg = prior_lognormal(log(0.1), 0.5),
            sigma = prior_lognormal(log(0.5), 0.5),
            centred = TRUE,
            scaled_variance = FALSE
        )
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = jlist@survival,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = jlist@longitudinal,
            formula = sld ~ time,
            threshold = 5
        )
    )


    set.seed(2213)
    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_warmup = 750,
            iter_sampling = 750,
            chains = 2,
            parallel_chains = 2
        )
    })

    summary_post <- function(model, vars, exp = FALSE) {
        dat <- model$summary(
            vars,
            mean = mean,
            q01 = \(x) purrr::set_names(quantile(x, 0.01), ""),
            q99 = \(x) purrr::set_names(quantile(x, 0.99), ""),
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
        cmdstanr::as.CmdStanMCMC(mp),
        c(
            "lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg",
            "lm_sf_sigma", "lm_sf_omega_bsld", "lm_sf_omega_kg", "lm_sf_omega_ks"
        )
    )
    true_values <- c(
        sim_params$mu_b, sim_params$mu_s, sim_params$mu_g,
        sim_params$sigma, sim_params$omega_b, sim_params$omega_g, sim_params$omega_s
    )
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})
