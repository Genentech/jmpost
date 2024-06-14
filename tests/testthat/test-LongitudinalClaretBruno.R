

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
    x@generated_quantities <- ""
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
    x@generated_quantities <- ""
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
    x@generated_quantities <- ""
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
    x@generated_quantities <- ""
    expect_stan_syntax(x)
})



test_that("Can recover known distributional parameters from a SF joint model", {

    skip_if_not(is_full_test())

    set.seed(2438)
    ## Generate Test data with known parameters
    jlist <- SimJointData(
        design = list(
            SimGroup(250, "Arm-A", "Study-X"),
            SimGroup(250, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalClaretBruno(
            times = c(
                -200, -100, -10,
                1, 100, 200, 300, 400, 500,
                600, 700, 800, 900, 1000, 1100
            ) / 365,
            sigma = 0.05,
            mu_b = log(60),
            mu_g = log(c(0.9, 1.1)),
            mu_c = log(c(0.45, 0.35)),
            mu_p = log(c(2.4, 1.8)),
            omega_b = 0.12,
            omega_g = 0.12,
            omega_c = 0.12,
            omega_p = 0.12,
            link_ttg = 0,
            link_dsld = -0.2,
            link_identity = 0,
            link_growth = 0
        ),
        survival = SimSurvivalExponential(
            time_max = 4,
            time_step = 1 / 365,
            lambda = 0.5,
            lambda_cen = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = -0.1,
                "C" = 0.6
            ),
            beta_cont = 0.25
        ),
        .silent = TRUE
    )


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
            linkDSLD(prior_normal(0, 0.5))
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
    set.seed(223)
    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 1000,
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
        as.CmdStanMCMC(mp),
        c("lm_clbr_mu_b", "lm_clbr_mu_g", "lm_clbr_mu_c", "lm_clbr_mu_p"),
        TRUE
    )
    true_values <- c(60, 0.9, 1.1, 0.45, 0.35, 2.4, 1.8)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))




    dat <- summary_post(
        as.CmdStanMCMC(mp),
        c("beta_os_cov", "sm_exp_lambda", "link_ttg")
    )
    true_values <- c(-0.1, 0.6, 0.25, 0.5, 0.2)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})
