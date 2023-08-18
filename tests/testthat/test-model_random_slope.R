

test_that("Random Slope Model can recover known parameter values", {
    ## Generate data with known parameters
    set.seed(739)
    jlist <- simulate_joint_data(
        n = c(250, 150),
        times = 1:2000,
        lambda_cen = 1 / 9000,
        lm_fun = sim_lm_random_slope(
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2,
            phi = 0
        ),
        os_fun = sim_os_exponential(1 / 100),
        .debug = TRUE,
        .silent = TRUE
    )


    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(0, 1, 100, 200, 250, 300, 350)) |>
        dplyr::arrange(pt, time)


    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        )
    )


    jdat <- DataJoint(
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ cov_cat + cov_cont,
            subject = "pt",
            arm = "arm",
            study = "study",
            time_grid = c(1)
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            subject = "pt",
            threshold = 5,
            time_grid = c(1)
        )
    )

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_sampling = 200,
        iter_warmup = 200,
        chains = 1,
        parallel_chains = 1
    )


    vars <- c(
        "lm_rs_intercept",       # 30
        "lm_rs_slope_mu",        # 1 , 3
        "lm_rs_slope_sigma"      # 0.2
        #"lm_rs_sigma"           # Ignored pending solving issue #185
    )

    pars <- mp@results$summary(vars)


    ## Check that we can recover main effects parameters
    z_score <- (c(30, 1, 3, 0.2) - pars$mean) / pars$sd
    expect_true(all(abs(z_score) < qnorm(0.95)))


    ## Check that we can recover random effects parameters
    pars <- suppressWarnings({
        mp@results$summary("lm_rs_ind_rnd_slope")$mean
    })

    ## Extract real random effects per patient
    ## We store them as (random effect + mean)
    ## thus need to subtract mean for comparison
    ## to nle4
    group_mean <- c("Group-1" = 1, "Group-2" = 3)
    dat_lm_unique <- dat_lm |>
        dplyr::distinct(.data$pt, .data$slope_ind, .data$arm) |>
        dplyr::mutate(group_mean = group_mean[.data$arm]) |>
        dplyr::mutate(slope_ind_offset = .data$slope_ind - .data$group_mean)

    stan_cor <- cor(pars, dat_lm_unique$slope_ind)
    expect_gt(stan_cor, 0.95)

    ## Check for consistency of random effects with lmer
    mod <- lme4::lmer(
        sld ~ time:arm + (time - 1 | pt),
        dat_lm
    )
    lmer_cor <- cor(
        lme4::ranef(mod)$pt$time,
        dat_lm_unique$slope_ind_offset
    )
    expect_gt(lmer_cor, 0.99)
})
