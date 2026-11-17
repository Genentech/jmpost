test_that("priorsense analyses jmpost draws with both density components", {
    skip_if_not(is_full_test())
    skip_if_not_installed("priorsense")

    subject <- data.frame(
        subject = paste0("S", 1:8),
        arm = "A",
        study = "X",
        time = c(1, 2, 1.5, 3, 2.5, 4, 3.5, 2),
        event = c(1, 0, 1, 1, 0, 1, 0, 1),
        x = rep(c(-0.5, 0.5), 4)
    )
    longitudinal <- data.frame(
        subject = rep(subject$subject, each = 3),
        time = rep(c(0, 0.25, 0.5), 8),
        y = c(
            1.8,
            2.1,
            2.2,
            2.3,
            2.0,
            2.1,
            1.9,
            2.0,
            2.4,
            2.2,
            2.4,
            2.1,
            1.7,
            1.9,
            2.0,
            2.0,
            2.3,
            2.5,
            1.9,
            1.8,
            2.1,
            2.1,
            2.2,
            2.0
        )
    )
    # Unequal visit counts and interleaved records exercise patient grouping.
    longitudinal <- longitudinal[setdiff(1:24, c(5, 6, 20, 23, 24)), ]
    longitudinal <- longitudinal[
        c(
            seq(2, nrow(longitudinal), by = 2),
            seq(1, nrow(longitudinal), by = 2)
        ),
    ]
    data <- DataJoint(
        DataSubject(subject, "subject", "arm", "study"),
        survival = DataSurvival(subject, Surv(time, event) ~ x),
        longitudinal = DataLongitudinal(longitudinal, y ~ time, threshold = 1.8)
    )
    model <- JointModel(
        LongitudinalRandomSlope(
            intercept = prior_normal(2, 1),
            slope_mu = prior_normal(0, 1),
            slope_sigma = prior_lognormal(-1, 0.5),
            sigma = prior_invgamma(3, 2, transform = prior_transform_square())
        ),
        SurvivalExponential(
            lambda = prior_gamma(2, 2),
            beta = prior_normal(0, 0.5)
        ),
        linkDSLD(prior = prior_normal(0, 0.5))
    )
    samples <- run_quietly(sampleStanModel(
        model,
        data = data,
        seed = 3519,
        chains = 2,
        parallel_chains = 2,
        iter_warmup = 500,
        iter_sampling = 300,
        adapt_delta = 0.95,
        refresh = 0,
        sig_figs = 16
    ))
    fit <- cmdstanr::as.CmdStanMCMC(samples)
    draws <- fit$draws(format = "draws_matrix")
    expect_true(all(is.finite(draws)))

    # Check the saved prior independently, including random effects and the
    # variance-scale prior's Jacobian and the exponential rate's truncation.
    expected_prior <- apply(draws, 1, function(x) {
        variance <- x["lm_rs_sigma"]^2
        dnorm(x["lm_rs_intercept[1]"], 2, 1, log = TRUE) +
            dnorm(x["lm_rs_slope_mu[1]"], 0, 1, log = TRUE) +
            dlnorm(x["lm_rs_slope_sigma[1]"], -1, 0.5, log = TRUE) +
            dgamma(1 / variance, 3, rate = 2, log = TRUE) -
            2 * log(variance) +
            log(2 * x["lm_rs_sigma"]) +
            sum(dnorm(
                x[paste0("lm_rs_ind_rnd_slope[", 1:8, "]")],
                x["lm_rs_slope_mu[1]"],
                x["lm_rs_slope_sigma[1]"],
                log = TRUE
            )) +
            dgamma(x["sm_exp_lambda"], 2, rate = 2, log = TRUE) -
            log(
                pgamma(8000, 2, rate = 2) -
                    pgamma(getOption("jmpost.double_eps"), 2, rate = 2)
            ) +
            dnorm(x["beta_os_cov[1]"], 0, 0.5, log = TRUE) +
            dnorm(x["link_dsld"], 0, 0.5, log = TRUE)
    })
    expect_equal(as.numeric(draws[, "lprior"]), unname(expected_prior))

    likelihood <- fit$draws("log_lik", format = "draws_matrix")
    survival_likelihood <- fit$draws("os_subj_log_lik", format = "draws_matrix")
    observation_likelihood <- fit$draws(
        "long_obvs_log_lik",
        format = "draws_matrix"
    )
    longitudinal_likelihood <- fit$draws(
        "long_subj_log_lik",
        format = "draws_matrix"
    )
    subject_index <- as_stan_list(data, model)$subject_tumour_index
    expected_longitudinal <- vapply(
        seq_len(nrow(subject)),
        function(i) {
            rowSums(observation_likelihood[, subject_index == i, drop = FALSE])
        },
        numeric(nrow(likelihood))
    )
    expect_equal(ncol(likelihood), nrow(subject))
    expect_equal(ncol(survival_likelihood), nrow(subject))
    expect_equal(ncol(longitudinal_likelihood), nrow(subject))
    expect_equal(ncol(observation_likelihood), nrow(longitudinal))
    expect_equal(
        as.numeric(longitudinal_likelihood),
        as.numeric(expected_longitudinal)
    )
    expect_equal(
        as.numeric(likelihood),
        as.numeric(survival_likelihood) + as.numeric(expected_longitudinal)
    )
    expect_equal(
        unname(rowSums(likelihood)),
        unname(rowSums(survival_likelihood) + rowSums(observation_likelihood))
    )
    expect_equal(
        as.numeric(priorsense::log_prior_draws(fit, joint = TRUE)),
        as.numeric(draws[, "lprior"])
    )
    expect_equal(
        unname(rowSums(posterior::as_draws_matrix(
            priorsense::log_lik_draws(fit)
        ))),
        unname(rowSums(likelihood))
    )

    sensitivity <- priorsense::powerscale_sensitivity(
        fit,
        variables = c("sm_exp_lambda", "link_dsld")
    )
    expect_setequal(sensitivity$variable, c("sm_exp_lambda", "link_dsld"))
    expect_true(all(is.finite(sensitivity$prior)))
    expect_true(all(is.finite(sensitivity$likelihood)))
})


test_that("saved prior retains vector truncation and horseshoe normalisation", {
    skip_if_not(is_full_test())

    parameters <- ParameterList(
        Parameter(
            set_limits(prior_horseshoe(scale_global = 0.5), -1, 1),
            "beta",
            size = 2
        ),
        Parameter(
            set_limits(prior_normal_vector(c(-1, 1), c(0.7, 1.2)), -2, 3),
            "alpha",
            size = 2
        ),
        Parameter(set_limits(prior_normal(0, 2), upper = 2), "upper_only"),
        Parameter(prior_std_normal(), "z"),
        Parameter(
            prior_normal(0, 1, transform = prior_transform_exp()),
            "empty",
            size = "n"
        )
    )
    priors <- as.StanModule(parameters)
    module <- merge(
        merge(StanModule("base/functions.stan"), priors),
        StanModule(paste0(
            "data {\n    int<lower=0> n;\n}\n",
            "generated quantities {\n    real lprior = 0;\n    {\n",
            stan_prior_density(paste(priors@model, collapse = "\n")),
            "\n    }\n}"
        ))
    )
    model <- compileStanModel(module)
    baseline <- list(
        beta = c(0.2, -0.4),
        prior_local_beta = c(0.5, 1.5),
        prior_global_beta = 0.3,
        prior_slab_beta = 1.2,
        alpha = c(-0.5, 1.5),
        upper_only = 0.5,
        z = -0.2,
        empty = numeric()
    )
    alternative <- baseline
    alternative$prior_global_beta <- 1.3
    alternative$alpha <- c(0.5, -0.5)
    alternative$upper_only <- -1
    alternative$z <- 0.7
    suppressMessages(capture.output(
        fit <- model$sample(
            data = c(list(n = 0L), as_stan_list(parameters)),
            init = list(baseline, alternative),
            fixed_param = TRUE,
            chains = 2,
            parallel_chains = 2,
            iter_warmup = 0,
            iter_sampling = 1,
            seed = 719,
            refresh = 0,
            sig_figs = 16
        )
    ))
    density <- function(x) {
        local <- x$prior_local_beta
        global <- x$prior_global_beta
        slab <- x$prior_slab_beta
        scales <- global *
            sqrt(
                (4 * slab * local^2) / (4 * slab + global^2 * local^2)
            )
        sum(dt(local, df = 1, log = TRUE) + log(2)) +
            dt(global / 0.5, df = 1, log = TRUE) -
            log(0.5) +
            log(2) +
            dgamma(1 / slab, 2, rate = 2, log = TRUE) -
            2 * log(slab) +
            sum(
                dnorm(x$beta, 0, scales, log = TRUE) -
                    log(pnorm(1, 0, scales) - pnorm(-1, 0, scales))
            ) +
            sum(
                dnorm(x$alpha, c(-1, 1), c(0.7, 1.2), log = TRUE) -
                    log(
                        pnorm(3, c(-1, 1), c(0.7, 1.2)) -
                            pnorm(-2, c(-1, 1), c(0.7, 1.2))
                    )
            ) +
            dnorm(x$upper_only, 0, 2, log = TRUE) -
            pnorm(2, 0, 2, log.p = TRUE) +
            dnorm(x$z, log = TRUE)
    }
    saved <- as.numeric(fit$draws("lprior"))
    expect_equal(
        saved,
        c(density(baseline), density(alternative)),
        tolerance = 1e-8
    )
})
