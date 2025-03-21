
#
# Setup global data objects to be used within this test file
#

set_fixtures_gsf <- function() {
    set.seed(739)
    jlist <- SimJointData(
        design = list(
            SimGroup(50, "Arm-A", "Study-X"),
            SimGroup(30, "Arm-B", "Study-Y"),
            SimGroup(30, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / (400 / 365),
            time_max = 4,
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
            times = seq(0, 4, by = 1 / 365),
            sigma = 0.01,
            mu_s = log(c(0.6, 0.4)),
            mu_g = log(c(0.25, 0.35)),
            mu_b = log(c(60, 50)),
            mu_phi = qlogis(c(0.4, 0.6)),
            omega_b = 0.2,
            omega_s = 0.2,
            omega_g = 0.2,
            omega_phi = 0.2
        ),
        .silent = TRUE
    )


    dat_os <- jlist@survival
    dat_lm <- jlist@longitudinal |>
        dplyr::group_by(subject) |>
        dplyr::sample_n(9) |>
        dplyr::group_by(subject) |>
        dplyr::filter(!subject == "subject_0004" | seq_len(dplyr::n()) <= 7) |>
        dplyr::ungroup()


    jm <- JointModel(
        longitudinal = LongitudinalGSF(
            mu_bsld = prior_normal(log(60), 0.5),
            mu_ks = prior_normal(log(0.6), 0.5),
            mu_kg = prior_normal(log(0.3), 0.5),
            mu_phi = prior_normal(plogis(0.5), 0.5),
            omega_bsld = prior_lognormal(log(0.2), 0.5),
            omega_ks = prior_lognormal(log(0.2), 0.5),
            omega_kg = prior_lognormal(log(0.2), 0.5),
            omega_phi = prior_lognormal(log(0.2), 0.5),
            sigma = prior_lognormal(log(0.01), 0.5),
            centred = TRUE
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / (400 / 365)), 1)
        ),
        link = Link()
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "subject",
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

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 100,
            iter_warmup = 100,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    fixtures_gsf <- new.env()
    fixtures_gsf$mp <- mp
    fixtures_gsf$jdat <- jdat
    fixtures_gsf$dat_os <- dat_os
    fixtures_gsf$dat_lm <- dat_lm
    fixtures_gsf$jlist <- jlist
    return(fixtures_gsf)

}

set_fixtures_rs <- function() {
    set.seed(739)
    jlist <- SimJointData(
        design = list(
            SimGroup(50, "Arm-A", "Study-X"),
            SimGroup(30, "Arm-B", "Study-Y"),
            SimGroup(30, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / (400 / 365),
            time_max = 4,
            time_step = 1 / 365,
            lambda_censor = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = -0.1,
                "C" = 0.5
            ),
            beta_cont = 0.3
        ),
        longitudinal = SimLongitudinalRandomSlope(
            seq(-50, 600, by = 50),
            intercept = c(50, 60),
            slope_mu = c(0.05, 0.02),
            slope_sigma = c(0.1, 0.2),
            sigma = 0.01
        ),
        .silent = TRUE
    )


    dat_os <- jlist@survival
    dat_lm <- jlist@longitudinal |>
        dplyr::group_by(subject) |>
        dplyr::sample_n(9) |>
        dplyr::group_by(subject) |>
        dplyr::filter(!subject == "subject_0004" | seq_len(dplyr::n()) <= 7) |>
        dplyr::ungroup()


    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(50, 10),
            slope_mu = prior_normal(0.03, 0.01),
            slope_sigma = prior_lognormal(log(0.15), 0.5),
            sigma = prior_lognormal(log(0.01), 0.5)
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / (400 / 365)), 1)
        ),
        link = Link()
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "subject",
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

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 100,
            iter_warmup = 100,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    fixtures_rs <- new.env()
    fixtures_rs$mp <- mp
    fixtures_rs$jdat <- jdat
    fixtures_rs$dat_os <- dat_os
    fixtures_rs$dat_lm <- dat_lm
    fixtures_rs$jlist <- jlist
    return(fixtures_rs)

}


fixtures_gsf <- set_fixtures_gsf()
fixtures_rs <- set_fixtures_rs()


test_that("Grid objects work with QuantityGenerator and QuantityCollapser", {
    dat_os <- dplyr::tibble(
        subject = c("A", "B", "C", "D"),
        arm = c("Arm-A", "Arm-A", "Arm-B", "Arm-B"),
        study = c("Study-1", "Study-1", "Study-1", "Study-1"),
        time = c(110, 220, 42, 302),
        event = c(1, 1, 0, 1)
    )


    dat_lm <- dplyr::tibble(
        subject = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D"),
        time = c(1, 2, 3, 10, 20, 30, 100, 200, 300, 1000, 2000, 3000),
        sld = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    )

    dj <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ 1
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            threshold = 5
        )
    )

    #
    # GridFixed
    #
    grid <- GridFixed(
        subjects = c("A", "B", "D"),
        times = c(1, 4)
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("A", "B", "D", "A", "B", "D"),
        times = c(1, 1, 1, 4, 4, 4)
    )
    expect_equal(actual, expected)

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = c("A", "B", "D", "A", "B", "D"),
        times = c(1, 1, 1, 4, 4, 4),
        indexes = list(1, 2, 3, 4, 5, 6)
    )
    expect_equal(actual, expected)


    #
    # GridGrouped
    #
    grid <- GridGrouped(
        groups = list("G1" = c("A", "D"), "G2" = c("A", "B")),
        times = c(1, 4)
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("A", "D", "B", "A", "D", "B"),
        times = c(1, 1, 1, 4, 4, 4)
    )
    expect_equal(actual, expected)

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = c("G1", "G2", "G1", "G2"),
        times = c(1, 1, 4, 4),
        indexes = list(c(1, 2), c(1, 3), c(4, 5), c(4, 6))
    )
    expect_equal(actual, expected)


    #
    # GridObserved
    #
    grid <- GridObserved(
        subjects = c("C", "A")
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("C", "C", "C", "A", "A", "A"),
        times = c(100, 200, 300, 1, 2, 3)
    )
    expect_equal(actual, expected)

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = c("C", "C", "C", "A", "A", "A"),
        times = c(100, 200, 300, 1, 2, 3),
        indexes = list(1, 2, 3, 4, 5, 6)
    )
    expect_equal(actual, expected)


    #
    # GridManual
    #
    grid <- GridManual(
        spec = list(
            "B" = c(2, 4),
            "A" = c(1, 10, 50),
            "C" = 6
        )
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("B", "B", "A", "A", "A", "C"),
        times = c(2, 4, 1, 10, 50, 6)
    )
    expect_equal(actual, expected)

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = c("B", "B", "A", "A", "A", "C"),
        times = c(2, 4, 1, 10, 50, 6),
        indexes = list(1, 2, 3, 4, 5, 6)
    )
    expect_equal(actual, expected)



    #
    # GridEven
    #
    grid <- GridEven(
        subjects = c("D", "A"),
        length.out = 4
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("D", "D", "D", "D", "A", "A", "A", "A"),
        times = c(
            seq(1000, 3000, length.out = 4),
            seq(1, 3, length.out = 4)
        )
    )
    expect_equal(actual, expected)

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = expected@subjects,
        times = expected@times,
        indexes = as.list(seq_along(expected@times))
    )
    expect_equal(actual, expected)


    #
    # GridEvent
    #
    grid <- GridEvent(
        subjects = c("D", "A", "B")
    )
    # Simple comparisons against an identical grid manual
    grid_man <- GridManual(
        spec = list(
            "D" = 302,
            "A" = 110,
            "B" = 220
        )
    )
    actual <- as.QuantityGenerator(grid, data = dj)
    expected <- QuantityGeneratorSubject(
        subjects = c("D", "A", "B"),
        times = c(302, 110, 220)
    )
    expect_equal(actual, expected)
    expect_equal(
        actual,
        as.QuantityGenerator(grid_man, data = dj)
    )

    actual <- as.QuantityCollapser(grid, data = dj)
    expected <- QuantityCollapser(
        groups = expected@subjects,
        times = expected@times,
        indexes = list(1, 2, 3)
    )
    expect_equal(actual, expected)
    expect_equal(
        actual,
        as.QuantityCollapser(grid_man, data = dj)
    )

    # Check that GridEvent errors if no survival data has been provided
    dj2 <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            threshold = 5
        )
    )
    expect_error(
        as.QuantityGenerator(grid, data = dj2),
        regexp = "`GridEvent\\(\\)`"
    )

})






test_that("GridObservered + Constructs correct quantities", {

    #
    # This test essentially works by attempting to construct the generated
    # quantities by hand and then testing to show that they match the values
    # generated by the jmpost functions
    #

    #
    #
    # Longitudinal Data
    #
    #

    # Testing that GridObserved returns sames results as GridManual assuming same time
    # specification
    longquant_obsv <- LongitudinalQuantities(
        fixtures_gsf$mp,
        grid = GridObserved(
            subjects = c("subject_0004", "subject_0002", "subject_0050")
        )
    )
    actual_obsv <- summary(longquant_obsv)


    longquant_manual <- LongitudinalQuantities(
        fixtures_gsf$mp,
        grid = GridManual(
            spec = list(
                "subject_0004" = fixtures_gsf$dat_lm |>
                    dplyr::filter(subject == "subject_0004") |>
                    dplyr::arrange(time) |>
                    dplyr::pull(time),
                "subject_0002" = fixtures_gsf$dat_lm |>
                    dplyr::filter(subject == "subject_0002") |>
                    dplyr::arrange(time) |>
                    dplyr::pull(time),
                "subject_0050" = fixtures_gsf$dat_lm |>
                    dplyr::filter(subject == "subject_0050") |>
                    dplyr::arrange(time) |>
                    dplyr::pull(time)
            )
        )
    )
    actual_manual <- summary(longquant_manual)

    expect_equal(actual_obsv, actual_manual)



    pred_mat <- cmdstanr::as.CmdStanMCMC(fixtures_gsf$mp)$draws("Ypred", format = "draws_matrix")

    fdat <- fixtures_gsf$dat_lm |>
        dplyr::arrange(subject, time, sld) |>
        dplyr::mutate(index = seq_len(dplyr::n())) |>
        dplyr::filter(subject %in% c("subject_0004", "subject_0002", "subject_0050"))

    times <- c(
        fdat |> dplyr::filter(subject == "subject_0004") |> dplyr::pull(time),
        fdat |> dplyr::filter(subject == "subject_0002") |> dplyr::pull(time),
        fdat |> dplyr::filter(subject == "subject_0050") |> dplyr::pull(time)
    )

    indexes <- c(
        fdat |> dplyr::filter(subject == "subject_0004") |> dplyr::pull(index),
        fdat |> dplyr::filter(subject == "subject_0002") |> dplyr::pull(index),
        fdat |> dplyr::filter(subject == "subject_0050") |> dplyr::pull(index)
    )

    preds_reduced <- pred_mat[, indexes]
    expected <- dplyr::tibble(
        group = rep(c("subject_0004", "subject_0002", "subject_0050"), c(7, 9, 9)),
        time = times,
        median = apply(preds_reduced, 2, median),
        lower = apply(preds_reduced, 2, quantile, 0.025),
        upper = apply(preds_reduced, 2, quantile, 0.975)
    )

    expect_gt(cor(actual_obsv$median, expected$median), 0.99999999)
    expect_gt(cor(actual_obsv$lower, expected$lower), 0.99999999)
    expect_gt(cor(actual_obsv$upper, expected$upper), 0.99999999)
    expect_equal(actual_obsv$time, expected$time)
    expect_equal(actual_obsv$group, expected$group)

    #
    #
    # Survival Data
    #
    #
    design <- model.matrix(~ cov_cat + cov_cont, data = fixtures_gsf$dat_os)

    beta_coefs <- cmdstanr::as.CmdStanMCMC(fixtures_gsf$mp)$draws(
        c("sm_exp_lambda", "beta_os_cov"),
        format = "draws_matrix"
    )
    beta_coefs[, 1] <- log(beta_coefs[, 1])

    lambda_samples <- exp(design %*% t(beta_coefs))[c(4, 2, 50), ]

    samples_df <- dplyr::tibble(
        subject = rep(c("subject_0004", "subject_0002", "subject_0050"), c(100, 100, 100)),
        id = rep(seq_len(100), 3),
        samples = c(lambda_samples[1, ], lambda_samples[2, ], lambda_samples[3, ])
    )

    expected <- purrr::map(
        c(0.25, 0.5, 0.75, 1.5, 2),
        \(time) {
            samples_df |> dplyr::mutate(time = time)
        }
    ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(surv = pexp(time, rate = samples, lower.tail = FALSE)) |>
        dplyr::group_by(subject, time) |>
        dplyr::summarise(
            median = median(surv),
            lower = quantile(surv, 0.025),
            upper = quantile(surv, 0.975),
            .groups = "drop"
        )

    survquant <- SurvivalQuantities(
        fixtures_gsf$mp,
        grid = GridFixed(
            subjects = c("subject_0004", "subject_0002", "subject_0050"),
            times = c(0.25, 0.5, 0.75, 1.50, 2)
        )
    )
    actual <- summary(survquant) |> dplyr::arrange(group, time)

    expect_gt(cor(actual$median, expected$median), 0.99999999)
    expect_gt(cor(actual$lower, expected$lower), 0.99999999)
    expect_gt(cor(actual$upper, expected$upper), 0.99999999)
    expect_equal(actual$time, expected$time)
    expect_equal(actual$group, expected$subject)
})


test_that("subjects_to_list works as expected", {
    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
        varm = c("A2", "A3", "A4"),
        vstudy = c("S1", "S1", "S2")
    )

    d_joint <- DataJoint(
        subject = DataSubject(
            data = df_subj,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        )
    )

    expect_equal(
        subjects_to_list(NULL, data = d_joint),
        list("C" = "C", "B" = "B", "A" = "A")
    )
    expect_equal(
        subjects_to_list(c("A", "B"), data = d_joint),
        list("A" = "A", "B" = "B")
    )
    expect_equal(
        subjects_to_list(c("B"), data = d_joint),
        list("B" = "B")
    )
    expect_error(
        subjects_to_list(c("B", "XX"), data = d_joint),
        regex = "Not all subjects exist within the data object"
    )
})


test_that("coalesceGridTime() works as expected", {
    grid <- GridFixed("A")
    grid2 <- coalesceGridTime(grid, c(1, 2, 3))
    expect_equal(grid2@times, c(1, 2, 3))

    grid <- GridFixed("A", 5)
    grid2 <- coalesceGridTime(grid, c(1, 2, 3))
    expect_equal(grid2@times, 5)

    grid <- GridGrouped(list("A" = "A"))
    grid2 <- coalesceGridTime(grid, c(1, 2, 3))
    expect_equal(grid2@times, c(1, 2, 3))

    grid <- GridGrouped(list("A" = "A"), 5)
    grid2 <- coalesceGridTime(grid, c(1, 2, 3))
    expect_equal(grid2@times, 5)
})



test_that("GridPopulation() works as expected for GSF models", {

    selected_times <- seq(-0.5, 4, by = 0.02)

    actual_quants <- LongitudinalQuantities(
        fixtures_gsf$mp,
        grid = GridPopulation(
            times = selected_times
        )
    )
    actual <- summary(actual_quants) |>
        dplyr::arrange(group, time)


    #
    # Derive values by hand
    #
    gsf_sld <- function(time, b, s, g, phi) {
        phi <- dplyr::if_else(time >= 0, phi, 0)
        b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
    }

    samples_df <- cmdstanr::as.CmdStanMCMC(fixtures_gsf$mp)$draws(
        c("lm_gsf_mu_ks", "lm_gsf_mu_kg", "lm_gsf_mu_bsld", "lm_gsf_mu_phi"),
        format = "draws_df"
    ) |>
        dplyr::as_tibble(.name_repair = make.names) |>
        dplyr::mutate(sample_id = seq_len(dplyr::n()))


    grouped_samples <- tidyr::crossing(
        samples_df,
        time = selected_times
    ) |>
        dplyr::mutate(
            esld_g1 = gsf_sld(
                time, exp(lm_gsf_mu_bsld.1.), exp(lm_gsf_mu_ks.1.), exp(lm_gsf_mu_kg.1.), plogis(lm_gsf_mu_phi.1.)
            ),
            esld_g2 = gsf_sld(
                time, exp(lm_gsf_mu_bsld.1.), exp(lm_gsf_mu_ks.2.), exp(lm_gsf_mu_kg.2.), plogis(lm_gsf_mu_phi.2.)
            ),
            esld_g3 = gsf_sld(
                time, exp(lm_gsf_mu_bsld.2.), exp(lm_gsf_mu_ks.2.), exp(lm_gsf_mu_kg.2.), plogis(lm_gsf_mu_phi.2.)
            ),
        ) |>
        dplyr::select(time, esld_g1, esld_g2, esld_g3, sample_id) |>
        tidyr::pivot_longer(cols = starts_with("esld"), names_to = "group", values_to = "sld") |>
        dplyr::mutate(group = factor(
            group,
            levels = c("esld_g1", "esld_g2", "esld_g3"),
            labels = c("arm=Arm-A; study=Study-X", "arm=Arm-B; study=Study-X", "arm=Arm-B; study=Study-Y")
        )) |>
        dplyr::mutate(group = as.character(group))

    expected <- grouped_samples |>
        dplyr::group_by(time, group) |>
        dplyr::summarise(
            lower = quantile(sld, 0.025),
            median = median(sld),
            upper = quantile(sld, 0.975),
            .groups = "drop"
        ) |>
        dplyr::select(group, time, median, lower, upper) |>
        dplyr::arrange(group, time)

    expect_gt(cor(actual$median, expected$median), 0.99999999)
    expect_gt(cor(actual$lower, expected$lower), 0.99999999)
    expect_gt(cor(actual$upper, expected$upper), 0.99999999)
    expect_equal(actual$time, expected$time)
    expect_equal(actual$group, expected$group)

})

test_that("GridPopulation() works as expected for Longitudinal models", {

    selected_times <- seq(-60, 400, by = 20)

    actual_quants <- LongitudinalQuantities(
        fixtures_rs$mp,
        grid = GridPopulation(
            times = selected_times
        )
    )
    actual <- summary(actual_quants) |>
        dplyr::arrange(group, time) |>
        dplyr::as_tibble()


    #
    # Derive values by hand
    #
    samples_df <- cmdstanr::as.CmdStanMCMC(fixtures_rs$mp)$draws(
        c("lm_rs_intercept", "lm_rs_slope_mu"),
        format = "draws_df"
    ) |>
        dplyr::as_tibble(.name_repair = make.names) |>
        dplyr::mutate(sample_id = seq_len(dplyr::n()))


    grouped_samples <- tidyr::crossing(
        samples_df,
        time = selected_times
    ) |>
        dplyr::mutate(
            esld_g1 = lm_rs_intercept.1. + lm_rs_slope_mu.1. * time,
            esld_g2 = lm_rs_intercept.1. + lm_rs_slope_mu.2. * time,
            esld_g3 = lm_rs_intercept.2. + lm_rs_slope_mu.2. * time
        ) |>
        dplyr::select(time, esld_g1, esld_g2, esld_g3, sample_id) |>
        tidyr::pivot_longer(cols = starts_with("esld"), names_to = "group", values_to = "sld") |>
        dplyr::mutate(group = factor(
            group,
            levels = c("esld_g1", "esld_g2", "esld_g3"),
            labels = c("arm=Arm-A; study=Study-X", "arm=Arm-B; study=Study-X", "arm=Arm-B; study=Study-Y")
        )) |>
        dplyr::mutate(group = as.character(group))

    expected <- grouped_samples |>
        dplyr::group_by(time, group) |>
        dplyr::summarise(
            lower = quantile(sld, 0.025),
            median = median(sld),
            upper = quantile(sld, 0.975),
            .groups = "drop"
        ) |>
        dplyr::select(group, time, median, lower, upper) |>
        dplyr::arrange(group, time)

    expect_gt(cor(actual$median, expected$median), 0.99999999)
    expect_gt(cor(actual$lower, expected$lower), 0.99999999)
    expect_gt(cor(actual$upper, expected$upper), 0.99999999)
    expect_equal(actual$time, expected$time)
    expect_equal(actual$group, expected$group)

})

test_that("GridPopulation() doesn't work with SurvivalQuantities", {
    expect_error(
        SurvivalQuantities(
            fixtures_gsf$mp,
            grid = GridPopulation(
                times = seq(1, 4, by = 0.02)
            )
        ),
        regex = "not supported"
    )
})


test_that("GridFixed() bug has been fixed", {
    long_quantities <- LongitudinalQuantities(
        fixtures_rs$mp,
        grid = GridFixed(
            subjects = c("subject_0001", "subject_0002")
        )
    )
    expect_class(long_quantities, "LongitudinalQuantities")
})
