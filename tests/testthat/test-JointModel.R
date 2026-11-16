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

test_that("horseshoe coefficients compose in joint and single-component models", {
    longitudinal <- LongitudinalSteinFojoCov(
        mu_s_coefficients_prior = prior_horseshoe(),
        mu_g_coefficients_prior = prior_horseshoe()
    )
    survival <- SurvivalWeibullPH(beta = prior_horseshoe())
    models <- list(
        JointModel(longitudinal, survival, link = linkDSLD()),
        JointModel(longitudinal = longitudinal),
        JointModel(survival = survival)
    )

    for (model in models) {
        module <- as.StanModule(model)
        expect_true(any(grepl("<multiplier=", module@parameters, fixed = TRUE)))
        expect_stan_syntax(module)
    }
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
snapshot_joint_model_parameter_decl <- function(object) {
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
        snapshot_joint_model_parameter_decl(JointModel(
            longitudinal = LongitudinalRandomSlope(),
            survival = SurvivalWeibullPH(),
            link = linkDSLD()
        ))
    )

    expect_snapshot(
        snapshot_joint_model_parameter_decl(JointModel(
            longitudinal = LongitudinalGSF(centred = FALSE),
            survival = SurvivalWeibullPH(),
            link = Link(linkTTG(), linkDSLD(), linkGrowth())
        ))
    )

    expect_snapshot(
        snapshot_joint_model_parameter_decl(JointModel(
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
    expect_true(grepl(
        "vector[n_subjects] log_lik = os_subj_log_lik + long_subj_log_lik;",
        stan_code, fixed = TRUE
    ))
    expect_false(grepl("log_lik = long_obvs_log_lik", stan_code))
    expect_false(grepl("log_lik = os_subj_log_lik;", stan_code, fixed = TRUE))

    x <- JointModel(
        longitudinal = LongitudinalRandomSlope()
    )
    stan_code <- as.character(x)
    expect_true(grepl("target \\+= sum\\(long_obvs_log_lik\\)", stan_code))
    expect_false(grepl("target \\+= sum\\(os_subj_log_lik\\)", stan_code))
    expect_true(grepl(
        "vector[n_subjects] log_lik = long_subj_log_lik;",
        stan_code, fixed = TRUE
    ))
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


test_that("built-in models render separate prior and likelihood densities", {
    longitudinal <- list(
        LongitudinalRandomSlope(), LongitudinalRandomSlopeCov(),
        LongitudinalGSF(), LongitudinalGSF(centred = TRUE),
        LongitudinalGSFCov(), LongitudinalSteinFojo(),
        LongitudinalSteinFojoCov(), LongitudinalClaretBruno(),
        LongitudinalClaretBrunoCov()
    )
    models <- c(
        lapply(longitudinal, function(x) {
            JointModel(x, SurvivalWeibullPH(), linkDSLD())
        }),
        list(
            JointModel(longitudinal = LongitudinalRandomSlope()),
            JointModel(survival = SurvivalExponential()),
            JointModel(survival = SurvivalGamma()),
            JointModel(survival = SurvivalLogLogistic()),
            JointModel(survival = SurvivalWeibullPH(beta = prior_horseshoe())),
            JointModel(survival = SurvivalExponential(
                lambda = prior_const(0.5), beta = prior_flat()
            )),
            JointModel(longitudinal = LongitudinalRandomSlope(
                sigma = prior_normal(2, 1, transform = prior_transform_square())
            ))
        )
    )
    for (model in models) {
        module <- as.StanModule(model)
        generated <- paste(module@generated_quantities, collapse = "\n")
        expect_match(generated, "real lprior = 0;", fixed = TRUE)
        expect_false(grepl("target|~", generated))
        expect_false(grepl("lprior += sum(", generated, fixed = TRUE))
        expect_stan_syntax(model)
    }
})
