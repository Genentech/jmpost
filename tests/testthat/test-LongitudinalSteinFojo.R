

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
    x@generated_quantities <- ""
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
    x@generated_quantities <- ""
    expect_stan_syntax(as.character(x))
})


test_that("Can recover known distributional parameters from a SF joint model", {

    skip_if_not(is_full_test())

    set.seed(9438)
    ## Generate Test data with known parameters
    jlist <- simulate_joint_data(
        n_arm = c(120, 120),
        times = seq(0, 4, by = (1 / 365) / 2),
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3,
        lm_fun = sim_lm_sf(
            sigma = 0.005,
            mu_s = c(0.2, 0.25),
            mu_g = c(0.15, 0.2),
            mu_b = 60,
            omega_b = 0.1,
            omega_s = 0.1,
            omega_g = 0.1,
            link_ttg = -0.2,
            link_dsld = 0.2
        ),
        os_fun = sim_os_weibull(
            lambda = 1,
            gamma = 1
        )
    )


    dat_os <- jlist$os
    select_times <- c(1, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900) * (1 / 365)
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% select_times) |>
        dplyr::arrange(pt, time)


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
            link_ttg(prior_normal(-0.2, 0.5)),
            link_dsld(prior_normal(0.2, 0.5))
        )
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ cov_cat + cov_cont
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            threshold = 5
        )
    )

    ## Sample from JointModel

    set.seed(2213)

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_sampling = 600,
        iter_warmup = 1000,
        chains = 2,
        parallel_chains = 2
    )

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
        mp@results,
        c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg"),
        TRUE
    )
    true_values <- c(60, 0.2, 0.25, 0.15, 0.2)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))

    dat <- summary_post(
        mp@results,
        c("link_dsld", "link_ttg", "sm_exp_lambda")
    )

    true_values <- c(0.2, -0.2, 1)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})
