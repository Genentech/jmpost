



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
