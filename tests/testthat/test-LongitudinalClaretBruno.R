

test_that("LongitudinalClaretBruno works as expected with default arguments", {
    result <- expect_silent(LongitudinalClaretBruno())
    expect_s4_class(result, "LongitudinalClaretBruno")
})



test_that("Print method for LongitudinalClaretBruno works as expected", {

    expect_snapshot({
        x <- LongitudinalClaretBruno()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalClaretBruno(
            sigma = prior_normal(0, 1),
            mu_g = prior_gamma(2, 1)
        )
        print(x)
    })
})


test_that("Centralised parameterisation compiles without issues", {
    jm <- JointModel(longitudinal = LongitudinalClaretBruno(centred = TRUE))
    expect_false(any(
        c("lm_clbr_eta_c", "lm_clbr_eta_p") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_clbr_ind_b", "lm_clbr_ind_g") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(as.character(x))
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(longitudinal = LongitudinalClaretBruno(centred = FALSE))
    expect_true(all(
        c("lm_clbr_eta_g", "lm_clbr_eta_b") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_clbr_ind_g", "lm_clbr_ind_b") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(as.character(x))
})



test_that("Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalClaretBruno(centred = TRUE),
        survival = SurvivalExponential(),
        link = Link(linkTTG(), linkDSLD(), linkGrowth())
    )
    expect_false(any(
        c("lm_clbr_eta_g", "lm_clbr_eta_b") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_clbr_ind_g", "lm_clbr_ind_b") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(
        longitudinal = LongitudinalClaretBruno(centred = FALSE),
        survival = SurvivalWeibullPH(),
        link = Link()
    )
    expect_true(all(
        c("lm_clbr_eta_g", "lm_clbr_eta_b") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_clbr_ind_g", "lm_clbr_ind_b") %in% names(jm@parameters)
    ))
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})



test_that("Can recover known distributional parameters from a SF joint model", {

    skip_if_not(is_full_test())


    sim_params <- list(
        sigma = 0.005,
        mu_b = log(60),
        mu_g = log(c(0.9, 1.1)),
        mu_c = log(c(0.45, 0.35)),
        mu_p = log(c(2.4, 1.8)),
        omega_b = 0.1,
        omega_g = c(0.3, 0.1),
        omega_c = c(0.1, 0.3),
        omega_p = c(0.3, 0.1),
        link_ttg = 0.3,
        link_dsld = -0.02,
        link_identity = 0,
        link_growth = 0,
        lambda = 0.5,
        lambda_cen = 1 / 9000,
        beta_cat_b = -0.1,
        beta_cat_c = 0.5,
        beta_cont = 0.3
    )

    set.seed(38)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(125, "Arm-A", "Study-X"),
            SimGroup(125, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalClaretBruno(
            times = c(
                1, 100, 200, 300, 400, 500,
                600, 700, 800, 900, 1000
            ) / 365,
            sigma = sim_params$sigma,
            mu_b = sim_params$mu_b,
            mu_g = sim_params$mu_g,
            mu_c = sim_params$mu_c,
            mu_p = sim_params$mu_p,
            omega_b = sim_params$omega_b,
            omega_g = sim_params$omega_g,
            omega_c = sim_params$omega_c,
            omega_p = sim_params$omega_p,
            link_ttg = sim_params$link_ttg,
            link_dsld = sim_params$link_dsld,
            link_identity = sim_params$link_identity,
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
        longitudinal = LongitudinalClaretBruno(

            mu_b = prior_normal(log(60), 0.4),
            mu_g = prior_normal(log(1), 0.4),
            mu_c = prior_normal(log(0.4), 0.4),
            mu_p = prior_normal(log(2), 0.4),

            omega_b = prior_lognormal(log(0.1), 0.4),
            omega_g = prior_lognormal(log(0.1), 0.4),
            omega_c = prior_lognormal(log(0.1), 0.4),
            omega_p = prior_lognormal(log(0.1), 0.4),

            sigma = prior_lognormal(log(0.05), 0.4),
            centred = TRUE

        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(0.5), 0.4),
            beta = prior_normal(0, 2)
        ),
        link = Link(
            linkDSLD(prior_normal(0, 0.5)),
            linkTTG(prior_normal(0, 0.5)),
            linkGrowth(prior_normal(10, 3))
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
    set.seed(663)
    mp <- run_quietly({
        suppressWarnings({
            sampleStanModel(
                jm,
                data = jdat,
                iter_sampling = 1000,
                iter_warmup = 1000,
                chains = 2,
                parallel_chains = 2
            )
        })
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
        c("lm_clbr_mu_b", "lm_clbr_mu_g", "lm_clbr_mu_c", "lm_clbr_mu_p"),
        TRUE
    )
    true_values <- exp(c(
        sim_params$mu_b, sim_params$mu_g, sim_params$mu_c, sim_params$mu_p
    ))
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))




    dat <- summary_post(
        cmdstanr::as.CmdStanMCMC(mp),
        c("beta_os_cov", "sm_exp_lambda", "link_dsld", "link_growth", "link_ttg")
    )
    true_values <- c(
        sim_params$beta_cat_b, sim_params$beta_cat_c, sim_params$beta_cont,
        sim_params$lambda,
        sim_params$link_dsld,
        sim_params$link_growth,
        sim_params$link_ttg
    )
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})


test_that("Quantity models pass the parser", {
    mock_samples <- .JointModelSamples(
        model = JointModel(longitudinal = LongitudinalClaretBruno()),
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
        "lm_clbr_omega_b", "lm_clbr_omega_g", "lm_clbr_omega_c",
        "lm_clbr_omega_p", "lm_clbr_sigma", "lm_gsf_sigma"
    )

    # Defaults work as expected
    mod <- LongitudinalClaretBruno()
    vals <- initialValues(mod, n_chains = 1)
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))


    # Test all individual parameters throw error if given prior that can't sample
    # valid value
    args <- list(
        omega_b = prior_normal(-200, 1),
        omega_g = prior_normal(-200, 1),
        omega_c = prior_normal(-200, 1),
        omega_p = prior_normal(-200, 1),
        sigma = prior_normal(-200, 1)
    )
    for (n_arg in names(args)) {
        arg <- args[n_arg]
        expect_error(
            {
                mod <- do.call(LongitudinalClaretBruno, arg)
                initialValues(mod, n_chains = 1)
            },
            regexp = "Unable to generate"
        )
    }

    # Test initial values can be found for weird priors that do overlap the valid region
    mod <- LongitudinalClaretBruno(
        omega_b = prior_normal(-200, 400),
        omega_g = prior_gamma(2, 5),
        omega_c = prior_uniform(-200, 400),
        omega_p = prior_lognormal(-200, 2),
        sigma = prior_cauchy(-200, 400)
    )
    set.seed(1001)
    vals <- unlist(initialValues(mod, n_chains = 200))
    vals <- vals[names(vals) %in% pars]
    expect_true(all(vals > 0))

})



test_that("Unscaled variance pass the parser", {
    jm <- JointModel(
        longitudinal = LongitudinalClaretBruno(centred = FALSE, scaled_variance = FALSE),
        survival = SurvivalLogLogistic(),
        link = linkDSLD()
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})




test_that("Can recover known distributional parameters from unscaled variance ClaretBruno model", {

    skip_if_not(is_full_test())


    sim_params <- list(
        sigma = 1,
        mu_b = log(60),
        mu_g = log(c(0.55, 0.65)),
        mu_c = log(c(0.45, 0.35)),
        mu_p = log(c(0.65, 0.75)),
        omega_b = 0.1,
        omega_g = c(0.1, 0.2),
        omega_c = c(0.2, 0.1),
        omega_p = c(0.1, 0.2),
        link_ttg = 0,
        link_dsld = 0,
        link_identity = 0,
        link_growth = 0,
        lambda = 0.5,
        lambda_cen = 1 / 9000,
        beta_cat_b = -0.1,
        beta_cat_c = 0.5,
        beta_cont = 0.3
    )

    set.seed(618)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(150, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalClaretBruno(
            times = c(1, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000) / 365,
            sigma = sim_params$sigma,
            mu_b = sim_params$mu_b,
            mu_g = sim_params$mu_g,
            mu_c = sim_params$mu_c,
            mu_p = sim_params$mu_p,
            omega_b = sim_params$omega_b,
            omega_g = sim_params$omega_g,
            omega_c = sim_params$omega_c,
            omega_p = sim_params$omega_p,
            link_ttg = sim_params$link_ttg,
            link_dsld = sim_params$link_dsld,
            link_identity = sim_params$link_identity,
            link_growth = sim_params$link_growth,
            scaled_variance = FALSE
        ),
        survival = SimSurvivalExponential(
            time_max = 5,
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
    # pdat <- jlist@longitudinal |> dplyr::filter(subject %in% sample(jlist@survival$subject, 5))
    # ggplot2::ggplot(pdat, aes(x = time, y = sld, col = subject, group = subject)) +
    #     geom_point() +
    #     geom_line()
    # nolint end


    jm <- JointModel(
        longitudinal = LongitudinalClaretBruno(
            mu_b = prior_normal(mean(sim_params$mu_b), 0.25),
            mu_g = prior_normal(mean(sim_params$mu_g), 0.25),
            mu_c = prior_normal(mean(sim_params$mu_c), 0.25),
            mu_p = prior_normal(mean(sim_params$mu_p), 0.25),
            omega_b = prior_lognormal(log(mean(sim_params$omega_b)), 0.25),
            omega_g = prior_lognormal(log(mean(sim_params$omega_g)), 0.25),
            omega_c = prior_lognormal(log(mean(sim_params$omega_c)), 0.25),
            omega_p = prior_lognormal(log(mean(sim_params$omega_p)), 0.25),
            sigma = prior_lognormal(log(mean(sim_params$sigma)), 0.25),
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
            threshold = 2
        )
    )

    ## Sample from JointModel
    set.seed(553)
    mp <- run_quietly({
        suppressWarnings({
            sampleStanModel(
                jm,
                data = jdat,
                iter_sampling = 2500,
                iter_warmup = 1000,
                chains = 3,
                parallel_chains = 3
            )
        })
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
        dat
    }

    par_names <- c(
        "mu_b", "mu_g", "mu_c", "mu_p",
        "omega_b", "omega_g", "omega_c", "omega_p",
        "sigma"
    )

    dat <- summary_post(
        cmdstanr::as.CmdStanMCMC(mp),
        paste0("lm_clbr_", par_names)
    )

    true_values <- sim_params[par_names] |> unlist()
    #### debug
    # dat$true_values <- true_values
    # dat$gt_q01 <- dat$q01 <= true_values
    # dat$lt_q99 <- dat$q99 >= true_values
    # dat[, c("variable", "true_values", "mean", "q01", "q99", "rhat", "ess_bulk", "ess_tail", "gt_q01", "lt_q99")] |> print()
    # 

    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))

})
