
test_that("Print method for LongitudinalRandomSlope works as expected", {

    expect_snapshot({
        x <- LongitudinalRandomSlope()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalRandomSlope(
            intercept = prior_normal(0, 1),
            sigma = prior_gamma(2, 1)
        )
        print(x)
    })
})

test_that("LongitudinalRandomSlope correctly generates an intercept per study", {
    set.seed(3321)

    real_intercept <- c(50, 70)
    real_slope_mu <- 10
    real_slope_sigma <- 0.5
    real_sigma <- 3

    sim_data <- SimJointData(
        design = list(
            SimGroup(80, "Arm-A", "Study-X"),
            SimGroup(120, "Arm-A", "Study-Y")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / 100,
            lambda_censor = 1 / 9000,
            time_max = 3,
            time_step = 1
        ),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(10, 50, 100, 300, 500, 900, 1200) / 365,
            intercept = real_intercept,
            slope_mu = real_slope_mu,
            slope_sigma = real_slope_sigma,
            sigma = real_sigma,
            link_dsld = 0,
            link_identity = 0
        ),
        .silent = TRUE
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = sim_data@survival,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = sim_data@longitudinal,
            formula = sld ~ time
        )
    )

    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(60, 15),
            slope_mu = prior_normal(10, 5),
            slope_sigma = prior_lognormal(log(0.5), 1),
            sigma = prior_lognormal(log(3), 1)
        ),
        link = Link()
    )

    suppressWarnings({
        mp <- run_quietly({
            sampleStanModel(
                jm,
                data = jdat,
                iter_warmup = 600,
                iter_sampling = 900,
                chains = 2,
                refresh = 0,
                parallel_chains = 2
            )
        })
    })

    samples <- mp@results$draws(
        c("lm_rs_intercept", "lm_rs_slope_mu", "lm_rs_slope_sigma", "lm_rs_sigma"),
        format = "draws_matrix"
    )

    ests_real <- c(
        real_intercept,
        real_slope_mu,
        real_slope_sigma,
        real_sigma
    )
    ests <- apply(samples, 2, mean)
    ests_se <- sqrt(apply(samples, 2, var))
    z_score <- (ests - ests_real) / ests_se
    expect_true(all(abs(z_score) < qnorm(0.99)))
})


test_that("Random Slope Model can recover known parameter values", {
    ## Generate data with known parameters
    set.seed(739)

    jlist <- SimJointData(
        design = list(
            SimGroup(250, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(1 / 100),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(0, 1, 100, 125, 200, 300, 350, 400, 500, 600),
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2
        ),
        .silent = TRUE
    )

    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        )
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = jlist@survival,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = jlist@longitudinal,
            formula = sld ~ time,
            threshold = 5
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 200,
            iter_warmup = 400,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })


    vars <- c(
        "lm_rs_intercept",       # 30
        "lm_rs_slope_mu",        # 1 , 3
        "lm_rs_slope_sigma",     # 0.2
        "lm_rs_sigma"            # 3
    )

    pars <- mp@results$summary(vars)


    ## Check that we can recover main effects parameters
    z_score <- (c(30, 1, 3, 0.2, 3) - pars$mean) / pars$sd
    expect_true(all(abs(z_score) < qnorm(0.95)))


    ## Check that we can recover random effects parameters
    pars <- suppressWarnings({
        mp@results$summary("lm_rs_ind_rnd_slope")$mean
    })

    ## Extract real random effects per patient
    ## We store them as (random effect + mean)
    ## thus need to subtract mean for comparison
    ## to nle4
    group_mean <- c(1, 3)

    ## Check for consistency of random effects with lmer
    mod <- lme4::lmer(
        sld ~ time:arm + (time - 1 | pt),
        jlist@longitudinal
    )
    lmer_cor <- cor(
        lme4::ranef(mod)$pt$time,
        pars - group_mean[as.numeric(jlist@survival$arm)]
    )
    expect_gt(lmer_cor, 0.99)
})



test_that("Random Slope Model left-censoring works as expected", {
    ## Generate data with known parameters
    set.seed(739)

    jlist <- SimJointData(
        design = list(
            SimGroup(250, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(1 / 100),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(-200, -150, -100, -50, 0, 1, 100, 125, 200, 300, 350, 400, 500, 600),
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2
        ),
        .silent = TRUE
    )


    # Trash the data for negative values
    # As the data is censored this shouldn't impact the sampled predictions
    dat_lm <- jlist@longitudinal
    negative_index <- dat_lm$sld < 0
    dat_lm$sld[negative_index] <- runif(sum(negative_index), -999, -100)

    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        )
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = jlist@survival,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            threshold = 0
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 200,
            iter_warmup = 400,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })


    vars <- c(
        "lm_rs_intercept",       # 30
        "lm_rs_slope_mu",        # 1 , 3
        "lm_rs_slope_sigma",     # 0.2
        "lm_rs_sigma"            # 3
    )

    pars <- mp@results$summary(vars)


    ## Check that we can recover main effects parameters
    z_score <- (c(30, 1, 3, 0.2, 3) - pars$mean) / pars$sd
    expect_true(all(abs(z_score) < qnorm(0.95)))


    ## Check that we can recover random effects parameters
    pars <- suppressWarnings({
        mp@results$summary("lm_rs_ind_rnd_slope")$mean
    })

    ## Extract real random effects per patient
    ## We store them as (random effect + mean)
    ## thus need to subtract mean for comparison
    ## to nle4
    group_mean <- c(1, 3)

    ## Check for consistency of random effects with lmer
    mod <- lme4::lmer(
        sld ~ time:arm + (time - 1 | pt),
        jlist@longitudinal
    )
    lmer_cor <- cor(
        lme4::ranef(mod)$pt$time,
        pars - group_mean[as.numeric(jlist@survival$arm)]
    )
    expect_gt(lmer_cor, 0.99)
})
