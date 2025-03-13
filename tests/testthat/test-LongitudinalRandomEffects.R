

test_that("LongitudinalRandomEffects() works as expected", {
    set.seed(1122)

    # Simulate data from first scratch
    n <- 200
    baseline <- dplyr::tibble(
        pt = factor(sprintf("pt_%04i", 1:n)),
        mu_b = 70,
        mu_s = 0.4,
        mu_g = 0.12,
        mu_phi = 0.5,
        sigma_b = 0.1,
        sigma_s = 0.1,
        sigma_g = 0.1,
        sigma_phi = 0.1,
        b = rlnorm(n, log(mu_b), sigma_b),
        s = rlnorm(n, log(mu_s), sigma_s),
        g = rlnorm(n, log(mu_g), sigma_g),
        phi = rbeta(n, 25, 25),
        sigma = 0.015
    )
    grid_df <- tidyr::expand_grid(
        pt = baseline$pt,
        time = seq(0, 6, length.out = 9)
    )
    dat_lm_all <- grid_df |>
        dplyr::left_join(baseline, by = "pt") |>
        dplyr::mutate(msld = gsf_sld(time, b, s, g, phi)) |>
        dplyr::mutate(sld = rnorm(nrow(grid_df), msld, msld * sigma))

    # Randomly shuffle to check if we have any implicit assumptions on
    # data already being sorted
    dat_lm <- dat_lm_all |>
        dplyr::select(time, pt, sld) |>
        dplyr::sample_frac(1)
    dat_subject <- baseline |>
        dplyr::select(pt) |>
        dplyr::mutate(arm = factor("A")) |>
        dplyr::mutate(study = factor("X")) |>
        dplyr::sample_frac(1)


    # Fit joint model to the data
    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_subject,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time
        )
    )
    jm <- JointModel(
        longitudinal = LongitudinalGSF(
            mu_bsld = prior_normal(log(mean(baseline$mu_b)), 0.25),
            mu_ks = prior_normal(log(mean(baseline$mu_s)), 0.25),
            mu_kg = prior_normal(log(mean(baseline$mu_g)), 0.25),
            mu_phi = prior_normal(qlogis(mean(baseline$mu_phi)), 0.25),
            omega_bsld = prior_lognormal(log(mean(baseline$sigma_b)), 0.25),
            omega_ks = prior_lognormal(log(mean(baseline$sigma_s)), 0.25),
            omega_kg = prior_lognormal(log(mean(baseline$sigma_g)), 0.25),
            omega_phi = prior_lognormal(log(mean(baseline$sigma_phi)), 0.25),
            sigma = prior_lognormal(log(mean(baseline$sigma)), 0.25),
            centred = TRUE,
            scaled_variance = TRUE
        )
    )
    suppressWarnings({
        mp <- run_quietly({
            sampleStanModel(
                jm,
                data = jdat,
                iter_warmup = 200,
                iter_sampling = 300,
                chains = 2,
                refresh = 200,
                parallel_chains = 2
            )
        })
    })

    # Extract patient level random effects and compare them to the true value
    requant <- LongitudinalRandomEffects(mp)
    s_requant <- summary(requant)
    combined <- baseline |>
        dplyr::select(pt, b, s, g, phi) |>
        tidyr::gather("parameter", "real", -pt) |>
        dplyr::left_join(s_requant, by = c("pt" = "subject", "parameter"))

    # Sensitivity check, showing that if ~3 values are slightly wrong the check fails
    # combined$real[1] <- 50
    # combined$real[2] <- 80
    # combined$real[3] <- 40
    expect_true(cor(combined$real, combined$median) > 0.9997)


    # Basic sanity checks
    expect_true(all(combined$lower < combined$median))
    expect_true(all(combined$median < combined$upper))

    # Show that if we recompute the medians by hand from the raw samples
    # that we get the values that were produced by `summary()`
    s_requant2 <- as.data.frame(requant) |>
        dplyr::group_by(subject, parameter) |>
        dplyr::summarise(median2 = median(values), .groups = "drop") |>
        dplyr::left_join(s_requant, by = c("subject", "parameter"))

    expect_equal(s_requant2$median, s_requant2$median2)

})
