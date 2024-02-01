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
    jm <- JointModel(longitudinal = LongitudinalGSF(centered = TRUE))
    expect_false(any(
        c("lm_gsf_eta_tilde_kg", "lm_gsf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_true(all(
        c("lm_gsf_psi_kg", "lm_gsf_psi_bsld") %in% names(jm@parameters)
    ))
    compileStanModel(jm)
})


test_that("Non-Centralised parameterisation compiles without issues", {
    jm <- JointModel(longitudinal = LongitudinalGSF(centered = FALSE))
    expect_true(all(
        c("lm_gsf_eta_tilde_kg", "lm_gsf_eta_tilde_bsld") %in% names(jm@parameters)
    ))
    expect_false(any(
        c("lm_gsf_psi_kg", "lm_gsf_psi_bsld") %in% names(jm@parameters)
    ))
    compileStanModel(jm)
})


test_that("Can recover known distributional parameters from a full GSF joint model", {

    skip_if_not(is_full_test())

    set.seed(7143)
    jlist <- simulate_joint_data(
        .debug = TRUE,
        n_arm = c(80, 100),
        times = seq(0, 3, by = (1 / 365) / 2),
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3,
        lm_fun = sim_lm_gsf(
            sigma = 0.01,
            mu_s = c(0.6, 0.4),
            mu_g = c(0.25, 0.35),
            mu_b = 60,
            a_phi = c(4, 6),
            b_phi = c(4, 6),
            omega_b = 0.2,
            omega_s = 0.2,
            omega_g = 0.2,
            link_dsld = 0.1,
            link_ttg = 0.2,
            link_identity = 0
        ),
        os_fun = sim_os_exponential(1 / (400 / 365))
    )

    set.seed(333)
    select_times <- sample(jlist$lm$time, 12)

    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% select_times) |>
        dplyr::arrange(time, pt)

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
            formula = sld ~ time
        )
    )

    jm <- JointModel(
        longitudinal = LongitudinalGSF(
            mu_bsld = prior_normal(log(60), 1),
            mu_ks = prior_normal(log(0.6), 1),
            mu_kg = prior_normal(log(0.3), 1),
            omega_bsld = prior_lognormal(log(0.2), 1),
            omega_ks = prior_lognormal(log(0.2), 1),
            omega_kg = prior_lognormal(log(0.2), 1),
            a_phi = prior_lognormal(log(6), 1),
            b_phi = prior_lognormal(log(8), 1),
            sigma = prior_lognormal(log(0.01), 1),
            centered = TRUE
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / (400 / 365)), 1)
        ),
        link = LinkGSF(
            link_gsf_dsld(),
            link_gsf_ttg()
        )
    )

    mp <- suppressWarnings(sampleStanModel(
        jm,
        data = jdat,
        iter_warmup = 400,
        iter_sampling = 800,
        chains = 2,
        refresh = 200,
        parallel_chains = 2
    ))

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
        mp@results,
        c("lm_gsf_mu_bsld", "lm_gsf_mu_ks", "lm_gsf_mu_kg"),
        TRUE
    )

    true_values <- c(60, 0.6, 0.4, 0.25, 0.35)
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))

    dat <- summary_post(
        mp@results,
        c("lm_gsf_beta", "lm_gsf_gamma", "lm_gsf_a_phi", "lm_gsf_b_phi", "sm_exp_lambda")
    )

    true_values <- c(0.1, 0.2, 4, 8, 4, 8, 1 / (1 / (400 / 365)))
    expect_true(all(dat$q01 <= true_values))
    expect_true(all(dat$q99 >= true_values))
    expect_true(all(dat$ess_bulk > 100))
})
