



test_that("JointModel smoke tests", {
    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalWeibullPH(),
        link = linkDSLD()
    )

    jm_char <- as.character(jm)
    expect_equal(length(jm_char), 1)
    expect_true(nchar(jm_char) > 3000)
})


test_that("JointModel print method works as expected", {
    expect_snapshot({
        x <- JointModel(
            longitudinal = LongitudinalRandomSlope(),
            survival = SurvivalWeibullPH(),
            link = linkDSLD()
        )
        print(x)
    })


    expect_snapshot({
        x <- JointModel(
            longitudinal = LongitudinalRandomSlope(),
            survival = SurvivalWeibullPH(),
            link = Link(
                linkDSLD(),
                linkIdentity()
            )
        )
        print(x)
    })

    expect_snapshot({
        x <- JointModel(
            survival = SurvivalWeibullPH()
        )
        print(x)
    })

    expect_snapshot({
        x <- JointModel(
            longitudinal = LongitudinalGSF()
        )
        print(x)
    })

    expect_snapshot({
        x <- JointModel(
            longitudinal = LongitudinalRandomSlope(),
            survival = SurvivalWeibullPH(),
            link = Link()
        )
        print(x)
    })
})



test_that("Log_Lik variables are produced correctly", {
    x <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalWeibullPH()
    )
    stan_code <- as.character(x)
    expect_true(grepl("target \\+= sum\\(long_obvs_log_lik\\)", stan_code))
    expect_true(grepl("target \\+= sum\\(os_subj_log_lik\\)", stan_code))
    expect_false(grepl("log_lik = long_obvs_log_lik", stan_code))
    expect_false(grepl("log_lik = os_subj_log_lik", stan_code))

    x <- JointModel(
        longitudinal = LongitudinalRandomSlope()
    )
    stan_code <- as.character(x)
    expect_true(grepl("target \\+= sum\\(long_obvs_log_lik\\)", stan_code))
    expect_false(grepl("target \\+= sum\\(os_subj_log_lik\\)", stan_code))
    expect_true(grepl("log_lik = long_obvs_log_lik", stan_code))
    expect_false(grepl("log_lik = os_subj_log_lik", stan_code))

    x <- JointModel(
        survival = SurvivalWeibullPH()
    )
    stan_code <- as.character(x)
    expect_false(grepl("target \\+=sum\\(long_obvs_log_lik\\)", stan_code))
    expect_true(grepl("target \\+= sum\\(os_subj_log_lik\\)", stan_code))
    expect_false(grepl("log_lik = long_obvs_log_lik", stan_code))
    expect_true(grepl("log_lik = os_subj_log_lik", stan_code))
})
