


test_that("link_none() works as expected", {
    expect_equal(
        Link(),
        linkNone()
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
            "matrix[n_subjects, 0] link_function_inputs = rep_matrix(0, n_subjects, 0);",
            as.character(x),
            fixed = TRUE
        )
    )
})


test_that("Link works as expected", {
    x <- Link(
        linkDSLD(prior_gamma(8, 2)),
        linkIdentity()
    )

    x <- resolvePromise(x, LongitudinalGSF())

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
            as.StanModule(x)
        )
    )

    # Check that function is idempotant
    expect_equal(
        Link(Link(linkDSLD())),
        Link(linkDSLD())
    )
})

test_that("Link prints as expected", {
    expect_snapshot(
        print(Link())
    )
    expect_snapshot(
        print(Link(linkDSLD()))
    )
    expect_snapshot(
        print(Link(linkDSLD(), linkIdentity()))
    )


    expect_snapshot({
        link <- resolvePromise(
            Link(linkDSLD(), linkIdentity()),
            LongitudinalGSF()
        )
        print(link)
    })

    expect_snapshot({
        link <- resolvePromise(
            Link(linkDSLD(), linkIdentity()),
            LongitudinalGSF()
        )
        print(link)
    })

})
