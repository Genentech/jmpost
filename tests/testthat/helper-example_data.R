
test_data_1 <- new.env()

ensure_test_data_1 <- function() {

    if (!is.null(test_data_1$jsamples)) {
        return(invisible(test_data_1))
    }

    set.seed(739)
    simjdat <- SimJointData(
        design = list(
            SimGroup(250, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / 100,
            time_max = 2000
        ),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(0, 1, 100, 200, 250, 300, 350),
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2,
            link_dsld = 0
        ),
        .silent = TRUE
    )

    dat_os <- simjdat@survival
    dat_lm <- simjdat@longitudinal


    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / 100), 1 / 100)
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
            formula = sld ~ time,
            threshold = 5
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 100,
            iter_warmup = 150,
            chains = 2,
            refresh = 0,
            parallel_chains = 1
        )
    })


    test_data_1$dat_os <- dat_os
    test_data_1$dat_lm <- dat_lm
    test_data_1$jmodel <- jm
    test_data_1$jdata <- jdat
    test_data_1$jsamples <- mp
    return(invisible(test_data_1))
}
