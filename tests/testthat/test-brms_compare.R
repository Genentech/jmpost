

test_that("jmpost and brms get similar loo for longitudinal models", {

    skip_if_not(is_full_test())

    set.seed(22231)
    n <- 250
    mu_b <- 60
    mu_s <- 0.5
    mu_g <- 0.2
    omega_b <- 0.1
    omega_s <- 0.1
    omega_g <- 0.1
    sigma <- 1.5
    n_vis <- 4

    sf_sld <- function(time, b, s, g) {
        s <- dplyr::if_else(time >= 0, s, 0)
        b * (exp(-s * time) + exp(g * time) - 1)
    }


    baseline <- dplyr::tibble(
        pt = sprintf("pt_%06i", seq_len(n)),
        b = rlnorm(n, log(mu_b), omega_b),
        s = rlnorm(n, log(mu_s), omega_s),
        g = rlnorm(n, log(mu_g), omega_g),
    )

    dat_full <- dplyr::tibble(
        pt = rep(sprintf("pt_%06i", seq_len(n)), each = n_vis),
        time = rep(seq(0, 3, length.out = n_vis), n)
    ) |>
        dplyr::left_join(baseline, by = "pt") |>
        dplyr::mutate(mu = sf_sld(time = time, b = b, s = s, g = g)) |>
        dplyr::mutate(value = rnorm(n * n_vis, mu, sigma))

    dat <- dat_full |>
        dplyr::select(pt, value, time)


    # nolint start
    # DEBUG
    # ggplot(data = filter(dat, pt %in% sample(dat$pt, 5)), aes(x = time, y = value, col = pt, group = pt)) +
    #     geom_point() +
    #     geom_line()
    # nolint end

    dat2 <- dat |>
        dplyr::mutate(arm = "A") |>
        dplyr::mutate(study = "A")

    dat_bl <- dat2 |>
        dplyr::select(pt, arm, study) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup()


    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_bl,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat2,
            formula =  value ~ time,
            threshold = -99999
        )
    )


    jm <- JointModel(
        longitudinal = LongitudinalSteinFojo(
            mu_bsld = prior_normal(log(mu_b), 0.6),
            mu_ks = prior_normal(log(mu_s), 0.6),
            mu_kg = prior_normal(log(mu_g), 0.6),
            omega_bsld = prior_lognormal(log(omega_b), 0.6),
            omega_ks = prior_lognormal(log(omega_s), 0.6),
            omega_kg = prior_lognormal(log(omega_g), 0.6),
            sigma = prior_lognormal(log(sigma), 0.6),
            centred = FALSE,
            scaled_variance = FALSE
        )
    )

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_warmup = 1400,
        iter_sampling = 2600,
        chains = 2,
        refresh = 200,
        parallel_chains = 2
    )


    stanmod <- cmdstanr::as.CmdStanMCMC(mp)


    mp_brms <- brms::brm(
        brms::bf(
            value ~ exp(b) * (exp(-exp(s) * time) + exp(exp(g) * time) - 1),
            b ~ 1 + (1 | pt),
            s ~ 1 + (1 | pt),
            g ~ 1 + (1 | pt),
            nl = TRUE
        ),
        data = dat,
        prior = c(
            brms::prior("normal(log(60), 0.6)", nlpar = "b"),
            brms::prior("normal(log(0.5), 0.6)", nlpar = "s"),
            brms::prior("normal(log(0.2), 0.6)", nlpar = "g"),
            brms::prior("lognormal(log(0.1), 0.6)", nlpar = "b", class = "sd"),
            brms::prior("lognormal(log(0.1), 0.6)", nlpar = "s", class = "sd"),
            brms::prior("lognormal(log(0.1), 0.6)", nlpar = "g", class = "sd"),
            brms::prior("lognormal(log(1.5), 0.6)", class = "sigma")
        ),
        warmup = 1400,
        iter = 2600,
        chains = 2,
        cores = 2,
        backend = "cmdstanr"
    )


    #
    # Assert that loo scores are similar
    #
    withCallingHandlers(
        b_est <- brms::loo(mp_brms),
        warning = function(w) {
            if (grepl("moment match", as.character(w))) { invokeRestart("muffleWarning") }
            # Else re-raise
            warning(w)
        }
    )

    withCallingHandlers(
        j_est <- stanmod$loo(),
        warning = function(w) {
            if (grepl("Pareto k diagnostic", as.character(w))) { invokeRestart("muffleWarning") }
            # Else re-raise
            warning(w)
        }
    )

    z_score <- abs(b_est$estimates[, "Estimate"] - j_est$estimates[, "Estimate"]) / b_est$estimates[, "SE"]
    expect_true(all(z_score < qnorm(0.99)))
    expect_true(cor(b_est$pointwise[, "elpd_loo"], j_est$pointwise[, "elpd_loo"]) > 0.95)


    #
    # Assert that patient level random effects are similar
    #
    bdat <- brms::as_draws_matrix(mp_brms) |> colMeans()

    cor_b <- cor(
        exp(bdat[grepl("r_pt__b.*Intercept", names(bdat))] + bdat["b_b_Intercept"]),
        posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_bsld")) |> colMeans()
    )

    cor_s <- cor(
        exp(bdat[grepl("r_pt__s.*Intercept", names(bdat))] + bdat["b_s_Intercept"]),
        posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_ks")) |> colMeans()
    )

    cor_g <- cor(
        exp(bdat[grepl("r_pt__g.*Intercept", names(bdat))] + bdat["b_g_Intercept"]),
        posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_kg")) |> colMeans()
    )

    expect_true(all(c(cor_b, cor_s, cor_g) > 0.999))
})






test_that("jmpost and brms get similar loo for survival models", {

    skip_if_not(is_full_test())

    set.seed(9825)
    n <- 500
    dat_surv <- dplyr::tibble(
        pt = sprintf("pt_%06i", seq_len(n)),
        lambda_0 = 365 / 150,
        cov1 = rnorm(n),
        cov2 = rnorm(n),
        lambda <- lambda_0 * exp(cov1 * 0.5 + cov2 * -0.3),
        time = rexp(n, 1 / lambda),
        event = 1
    )


    mp_brms <- brms::brm(
        time ~ 1 + cov1 + cov2,
        family = brms::exponential(),
        data = dat_surv,
        prior = c(),
        warmup = 2000,
        iter = 3000,
        chains = 2,
        cores = 2,
        backend = "cmdstanr"
    )


    dat_surv2 <- dat_surv |>
        dplyr::mutate(arm = "A") |>
        dplyr::mutate(study = "A")

    dat_surv_bl <- dat_surv2 |>
        dplyr::select(pt, arm, study) |>
        dplyr::group_by(pt) |>
        dplyr::slice(1) |>
        dplyr::ungroup()


    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_surv_bl,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = dat_surv2,
            formula =  Surv(time, event) ~ cov1 + cov2
        )
    )


    jm <- JointModel(
        survival = SurvivalExponential()
    )

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_warmup = 2000,
        iter_sampling = 3000,
        chains = 2,
        refresh = 200,
        parallel_chains = 2
    )

    stanmod <- cmdstanr::as.CmdStanMCMC(mp)


    #
    # Assert that loo scores are similar
    #
    b_est <- brms::loo(mp_brms)
    j_est <- stanmod$loo()

    z_score <- abs(b_est$estimates[, "Estimate"] - j_est$estimates[, "Estimate"]) / b_est$estimates[, "SE"]
    expect_true(all(z_score < qnorm(0.99)))
    expect_true(cor(b_est$pointwise[, "looic"], j_est$pointwise[, "looic"]) > 0.95)

})
