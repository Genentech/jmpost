


test_that("link_none() works as expected", {
    expect_equal(
        Link(),
        link_none()
    )

    expect_equal(
        length(Link()),
        0
    )


    # Double check that key "link_none" bits of stan code present in the complete joint model
    # if no link is provided
    x <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalWeibullPH(),
        link = Link()
    )
    expect_true(
        grepl(
            "return  rep_matrix(0, rows(time), cols(time));",
            as.character(x),
            fixed = TRUE
        )
    )
    expect_true(
        grepl(
            "matrix[Nind, 0] link_function_inputs = rep_matrix(0, Nind, 0);",
            as.character(x),
            fixed = TRUE
        )
    )
})


test_that("Link works as expected", {
    x <- Link(
        link_dsld(prior_gamma(8, 2)),
        link_identity()
    )

    expect_equal(
        length(x),
        2
    )

    expect_equal(
        getParameters(x),
        ParameterList(
            Parameter(prior_gamma(8, 2), "link_dsld", 1),
            Parameter(prior_normal(0, 2), "link_identity", 1)
        )
    )

    ## Fully resolved stan code pass's the syntax checker
    expect_stan_syntax(
        merge(
            load_with_base_stan("lm-gsf/functions.stan"),
            as.StanModule(x, model = LongitudinalGSF())
        )
    )

    # Check that function is idempotant
    expect_equal(
        Link(Link(link_dsld())),
        Link(link_dsld())
    )
})

test_that("Link prints as expected", {
    expect_snapshot(
        print(Link())
    )
    expect_snapshot(
        print(Link(link_dsld()))
    )
    expect_snapshot(
        print(Link(link_dsld(), link_identity()))
    )
})
