

test_that("all link files pass stan's syntax checker", {
    expect_stan_syntax(
        load_with_base_stan("lm-gsf/link_dsld.stan")
    )
    expect_stan_syntax(
        load_with_base_stan("lm-gsf/link_ttg.stan")
    )
    expect_stan_syntax(
        load_with_base_stan("lm-gsf/link_identity.stan", "lm-gsf/functions.stan")
    )


    expect_stan_syntax(
        load_with_base_stan("lm-random-slope/link_dsld.stan")
    )
    expect_stan_syntax(
        load_with_base_stan("lm-random-slope/link_identity.stan")
    )
})



test_that("complete models with links pass stan's syntax checker", {

    x <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalWeibullPH(),
        link = Link(link_dsld(), link_identity())
    )
    stan <- as.StanModule(x)
    # Currently generated quantities depends on data elements that aren't defined
    # Until later so void it out for now
    stan@generated_quantities <- ""
    expect_stan_syntax(stan)



    x <- JointModel(
        longitudinal = LongitudinalGSF(),
        survival = SurvivalLogLogistic(),
        link = Link(link_dsld(), link_identity(), link_ttg())
    )
    stan <- as.StanModule(x)
    # Currently generated quantities depends on data elements that aren't defined
    # Until later so void it out for now
    stan@generated_quantities <- ""
    expect_stan_syntax(stan)

})


test_that("LinkComponents are constructed correctly and can access key components", {
    par_list <- ParameterList(
        Parameter(prior = prior_normal(0, 5), name = "bobby", size = 1)
    )

    x <- LinkComponent(
        stan = \(model) {
            expect_class(model, "LongitudinalGSF")
            StanModule("lm-gsf/link_ttg.stan")
        },
        key = "bob",
        parameters = par_list
    )

    # Check that `stan` function arguments are called correctly
    expect_equal(
        as.StanModule(x, model = LongitudinalGSF()),
        StanModule("lm-gsf/link_ttg.stan")
    )

    expect_equal(
        getParameters(x),
        par_list
    )

    expect_equal(
        x@key,
        "bob"
    )
})


test_that("Model specific links return the correct stan code", {

    ### GSF

    expect_equal(
        as.StanModule(link_dsld(), model = LongitudinalGSF()),
        StanModule("lm-gsf/link_dsld.stan")
    )

    expect_equal(
        as.StanModule(link_identity(), model = LongitudinalGSF()),
        StanModule("lm-gsf/link_identity.stan")
    )

    expect_equal(
        as.StanModule(link_ttg(), model = LongitudinalGSF()),
        StanModule("lm-gsf/link_ttg.stan")
    )

    ### Random Slope

    expect_equal(
        as.StanModule(link_dsld(), model = LongitudinalRandomSlope()),
        StanModule("lm-random-slope/link_dsld.stan")
    )

    expect_equal(
        as.StanModule(link_identity(), model = LongitudinalRandomSlope()),
        StanModule("lm-random-slope/link_identity.stan")
    )

    expect_error(
        as.StanModule(link_ttg(), model = LongitudinalRandomSlope()),
        regexp = "TTG link is not available"
    )

})
