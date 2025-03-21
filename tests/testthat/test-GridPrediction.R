set_fixtures_gsf_link <- function() {
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
            omega_phi = 0.2,
            link_dsld = 0.04,
            link_identity = 0.04
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
        link = Link(
            linkDSLD(),
            linkTTG()
        )
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
            iter_sampling = 40,
            iter_warmup = 40,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    fixtures_gsf_link <- new.env()
    fixtures_gsf_link$mp <- mp
    fixtures_gsf_link$jdat <- jdat
    fixtures_gsf_link$dat_os <- dat_os
    fixtures_gsf_link$dat_lm <- dat_lm
    fixtures_gsf_link$jlist <- jlist
    return(fixtures_gsf_link)

}

set_fixtures_weibull_only <- function() {
    set.seed(739)
    jlist <- SimJointData(
        design = list(
            SimGroup(50, "Arm-A", "Study-X"),
            SimGroup(30, "Arm-B", "Study-Y"),
            SimGroup(30, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalWeibullPH(
            lambda = 1 / (400 / 365),
            gamma = 0.95,
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
            omega_phi = 0.2,
            link_dsld = 0,
            link_identity = 0
        ),
        .silent = TRUE
    )


    dat_os <- jlist@survival

    jm <- JointModel(
        survival = SurvivalWeibullPH(
            lambda = prior_lognormal(log(1 / (400 / 365)), 1),
            gamma = prior_lognormal(log(0.95), 1)
        )
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
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 200,
            iter_warmup = 200,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    fixtures_weibull_only <- new.env()
    fixtures_weibull_only$mp <- mp
    fixtures_weibull_only$jdat <- jdat
    fixtures_weibull_only$dat_os <- dat_os
    fixtures_weibull_only$jlist <- jlist
    return(fixtures_weibull_only)

}


fixtures_gsf_link <- set_fixtures_gsf_link()
fixtures_weibull_only <- set_fixtures_weibull_only()


test_that("GridPrediction() works as expected for Survival models", {

    selected_times <- seq(0, 200, by = 20) / 365

    new_data <- dplyr::tibble(
        cov_cont = c(1, 2, -1),
        cov_cat = c("A", "C", "B")
    )
    new_params <- list(
        "b" = 50,
        "s" = 0.6,
        "g" = 0.3,
        "phi" = 0.5
    )

    actual_quants <- SurvivalQuantities(
        fixtures_gsf_link$mp,
        type = "haz",
        grid = GridPrediction(
            times = selected_times,
            newdata = new_data,
            params = new_params
        )
    )
    actual <- summary(actual_quants) |>
        dplyr::arrange(group, time) |>
        dplyr::as_tibble()


    #
    # Derive values by hand
    #
    samples_df <- cmdstanr::as.CmdStanMCMC(fixtures_gsf_link$mp)$draws(
        c("beta_os_cov", "link_dsld", "link_ttg", "sm_exp_lambda"),
        format = "draws_df"
    ) |>
        dplyr::as_tibble(.name_repair = make.names) |>
        dplyr::mutate(sample_id = seq_len(dplyr::n()))

    grouped_samples <- tidyr::crossing(
        samples_df,
        time = selected_times
    ) |>
        dplyr::mutate(
            esld_g1 = sm_exp_lambda * exp(
                1 * beta_os_cov.3. +
                    link_dsld * gsf_dsld(time, new_params$b, new_params$s, new_params$g, new_params$phi) +
                    link_ttg * gsf_ttg(time, new_params$b, new_params$s, new_params$g, new_params$phi)
            ),
            esld_g2 =  sm_exp_lambda * exp(
                2 * beta_os_cov.3. + beta_os_cov.2. +
                    link_dsld * gsf_dsld(time, new_params$b, new_params$s, new_params$g, new_params$phi) +
                    link_ttg * gsf_ttg(time, new_params$b, new_params$s, new_params$g, new_params$phi)
            ),
            esld_g3 =  sm_exp_lambda * exp(
                -1 * beta_os_cov.3. + beta_os_cov.1. +
                    link_dsld * gsf_dsld(time, new_params$b, new_params$s, new_params$g, new_params$phi) +
                    link_ttg * gsf_ttg(time, new_params$b, new_params$s, new_params$g, new_params$phi)
            )
        ) |>
        dplyr::select(time, esld_g1, esld_g2, esld_g3, sample_id) |>
        tidyr::pivot_longer(cols = starts_with("esld"), names_to = "group", values_to = "haz") |>
        dplyr::mutate(group = factor(
            group,
            levels = c("esld_g1", "esld_g2", "esld_g3"),
            labels = c("new_subject_1", "new_subject_2", "new_subject_3")
        )) |>
        dplyr::mutate(group = as.character(group))

    expected <- grouped_samples |>
        dplyr::group_by(time, group) |>
        dplyr::summarise(
            lower = quantile(haz, 0.025),
            median = median(haz),
            upper = quantile(haz, 0.975),
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


test_that("GridPrediction() error handling works as expected", {

    selected_times <- seq(0, 200, by = 20) / 365

    new_data <- dplyr::tibble(
        cov_cont = c(1, 2, -1, 4),
        cov_cat = c("A", "C", "B", "XXX")
    )
    new_params <- list(
        "b" = 50,
        "s" = 0.6,
        "g" = 0.3,
        "phi" = 0.5
    )

    expect_error(
        SurvivalQuantities(
            fixtures_gsf_link$mp,
            type = "haz",
            grid = GridPrediction(times = selected_times, newdata = new_data, params = new_params)
        ),
        regex = "has new levels"
    )

    expect_error(
        SurvivalQuantities(
            fixtures_gsf_link$mp,
            type = "haz",
            grid = GridPrediction(times = c(-100, 100), newdata = new_data, params = new_params)
        ),
        regex = "greater than or equal to 0"
    )


    new_params <- list(
        "b" = c(10, 50),
        "s" = c(0.1, 0.6),
        "g" = c(0.1, 0.3),
        "phi" = c(0.2, 0.5)
    )
    expect_error(
        SurvivalQuantities(
            fixtures_gsf_link$mp,
            type = "haz",
            grid = GridPrediction(times = selected_times, newdata = new_data, params = new_params)
        ),
        regex = "must be length 1"
    )


    new_params <- list(
        "b" = c(10),
        "s" = c(0.1),
        "g" = c(0.1),
        "phi" = c(0.2, 0.5)
    )
    expect_error(
        SurvivalQuantities(
            fixtures_gsf_link$mp,
            type = "haz",
            grid = GridPrediction(times = selected_times, newdata = new_data, params = new_params)
        ),
        regex = "must be length 1"
    )
})



test_that("getPredictionNames() works as expected", {
    expect_equal(
        getPredictionNames(LongitudinalGSF()),
        c("b", "s", "g", "phi")
    )
    expect_equal(
        getPredictionNames(LongitudinalSteinFojo()),
        c("b", "s", "g")
    )
    expect_equal(
        getPredictionNames(LongitudinalRandomSlope()),
        c("intercept", "slope")
    )
})

test_that("GridPrediction() throws an error if key column already exists", {
    expect_error(
        GridPrediction(
            newdata = dplyr::tibble("..new_subject.." = 1, val = 5),
            times = c(1, 2, 3),
            params = list()
        ),
        regex = "'..new_subject..'"
    )
})


test_that("GridPrediction() works for survival only models", {

    selected_times <- seq(0, 200, by = 20) / 365

    new_data <- dplyr::tibble(
        cov_cont = c(1, 2, -1),
        cov_cat = c("A", "C", "B")
    )

    actual_quants <- SurvivalQuantities(
        fixtures_weibull_only$mp,
        type = "surv",
        grid = GridPrediction(
            times = selected_times,
            newdata = new_data
        )
    )
    actual <- summary(actual_quants) |>
        dplyr::arrange(group, time) |>
        dplyr::as_tibble()

    # Calculate expected values by hand
    pars_dat <- cmdstanr::as.CmdStanMCMC(fixtures_weibull_only$mp)$draws(
        c("beta_os_cov", "sm_weibull_ph_lambda", "sm_weibull_ph_gamma"),
        format = "draws_df"
    ) |>
        dplyr::as_tibble(.name_repair = make.names) |>
        dplyr::mutate(sample_id = seq_len(dplyr::n())) |>
        dplyr::mutate(new_subject_1 = sm_weibull_ph_lambda * exp(
            0 * beta_os_cov.1. +
                0 * beta_os_cov.2. +
                1 * beta_os_cov.3.
        )) |>
        dplyr::mutate(new_subject_2 = sm_weibull_ph_lambda * exp(
            0 * beta_os_cov.1. +
                1 * beta_os_cov.2. +
                2 * beta_os_cov.3.
        )) |>
        dplyr::mutate(new_subject_3 = sm_weibull_ph_lambda * exp(
            1 * beta_os_cov.1. +
                0 * beta_os_cov.2. +
                -1 * beta_os_cov.3.
        )) |>
        dplyr::select(gamma = sm_weibull_ph_gamma, new_subject_1, new_subject_2, new_subject_3, sample_id) |>
        tidyr::pivot_longer(cols = starts_with("new_subject"), names_to = "group", values_to = "lambda")

    expected <- tidyr::crossing(
        pars_dat,
        time = selected_times
    ) |>
        dplyr::mutate(surv = flexsurv::pweibullPH(time, shape = gamma, scale = lambda, lower.tail = FALSE)) |>
        dplyr::group_by(time, group) |>
        dplyr::summarise(
            lower = quantile(surv, 0.025),
            median = median(surv),
            upper = quantile(surv, 0.975),
            .groups = "drop"
        ) |>
        dplyr::arrange(group, time)

    expect_gt(cor(actual$median, expected$median), 0.999999)
    expect_gt(cor(actual$lower, expected$lower), 0.999999)
    expect_gt(cor(actual$upper, expected$upper), 0.999999)
    expect_equal(actual$time, expected$time)
    expect_equal(actual$group, expected$group)
})
