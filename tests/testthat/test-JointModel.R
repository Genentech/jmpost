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


# Keep declaration snapshots focused on semantic Stan declarations, rather than
# churn from blank lines or source comments introduced by StanModule merging.
normalise_stan_declarations <- function(x) {
    x <- trimws(x)
    x <- x[nzchar(x)]
    x[!grepl("^//", x)]
}

# Snapshot only the assembled parameter declarations and constant-parameter
# declarations. This catches regressions in dynamic parameter insertion without
# snapshotting the full generated Stan program.
snapshot_joint_model_parameter_declarations <- function(object) {
    stan <- as.StanModule(object)
    cat("parameters\n")
    cat(normalise_stan_declarations(stan@parameters), sep = "\n")

    const_declarations <- normalise_stan_declarations(
        stan@transformed_parameters
    )
    const_declarations <- const_declarations[grepl(
        "prior_const_",
        const_declarations,
        fixed = TRUE
    )]

    cat("\ntransformed parameter constants\n")
    if (length(const_declarations) == 0) {
        cat("<none>\n")
    } else {
        cat(const_declarations, sep = "\n")
    }
}


test_that("JointModel snapshots assembled parameter declarations", {
    expect_snapshot(
        snapshot_joint_model_parameter_declarations(JointModel(
            longitudinal = LongitudinalRandomSlope(),
            survival = SurvivalWeibullPH(),
            link = linkDSLD()
        ))
    )

    expect_snapshot(
        snapshot_joint_model_parameter_declarations(JointModel(
            longitudinal = LongitudinalGSF(centred = FALSE),
            survival = SurvivalWeibullPH(),
            link = Link(linkTTG(), linkDSLD(), linkGrowth())
        ))
    )

    expect_snapshot(
        snapshot_joint_model_parameter_declarations(JointModel(
            survival = SurvivalExponential(lambda = prior_const(0.5))
        ))
    )
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
