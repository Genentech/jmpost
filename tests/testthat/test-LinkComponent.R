

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
        link = Link(linkDSLD(), linkIdentity())
    )
    stan <- as.StanModule(x)
    # Currently generated quantities depends on data elements that aren't defined
    # Until later so void it out for now
    stan@generated_quantities <- ""
    expect_stan_syntax(stan)



    x <- JointModel(
        longitudinal = LongitudinalGSF(),
        survival = SurvivalLogLogistic(),
        link = Link(linkDSLD(), linkIdentity(), linkTTG())
    )
    stan <- as.StanModule(x)
    # Currently generated quantities depends on data elements that aren't defined
    # Until later so void it out for now
    stan@generated_quantities <- ""
    expect_stan_syntax(stan)

})


test_that("LinkComponents are constructed correctly and can access key components", {
    x <- LinkComponent(
        stan = StanModule("lm-gsf/link_ttg.stan"),
        key = "bob",
        prior = prior_normal(0, 5)
    )

    # Check that `stan` function arguments are called correctly
    expect_equal(
        as.StanModule(x, model = LongitudinalGSF()),
        StanModule("lm-gsf/link_ttg.stan")
    )

    expect_equal(
        getParameters(x),
        ParameterList(
            Parameter(prior = prior_normal(0, 5), name = "bob", size = 1)
        )
    )

    expect_equal(
        x@key,
        "bob"
    )
})


test_that("Model specific links return the correct stan code", {

    ### GSF

    expect_equal(
        as.StanModule(linkDSLD(model = LongitudinalGSF())),
        StanModule("lm-gsf/link_dsld.stan")
    )

    expect_equal(
        as.StanModule(linkIdentity(model = LongitudinalGSF())),
        StanModule("lm-gsf/link_identity.stan")
    )

    expect_equal(
        as.StanModule(linkTTG(model = LongitudinalGSF())),
        StanModule("lm-gsf/link_ttg.stan")
    )

    ### Random Slope

    expect_equal(
        as.StanModule(linkDSLD(model = LongitudinalRandomSlope())),
        StanModule("lm-random-slope/link_dsld.stan")
    )

    expect_equal(
        as.StanModule(linkIdentity(model = LongitudinalRandomSlope())),
        StanModule("lm-random-slope/link_identity.stan")
    )

    expect_error(
        as.StanModule(linkTTG(model = LongitudinalRandomSlope())),
        regexp = "Method `linkTTG` is not available"
    )

})


test_that("print works as expected", {
    expect_snapshot(
        print(linkDSLD())
    )
    expect_snapshot(
        print(linkTTG(prior_beta(4, 1)))
    )
    expect_snapshot(
        print(
            LinkComponent(
                stan = StanModule(),
                prior = prior_normal(0, 5),
                key = "bob"
            )
        )
    )
})


test_that("PromiseLinkComponents work as expected", {
    x <- PromiseLinkComponent(
        fun = \(prior, model, ...) {
            LinkComponent(
                stan = StanModule("lm-gsf/link_ttg.stan"),
                prior = prior,
                key = "bob"
            )
        },
        key = "bob",
        prior = prior_normal(0, 5)
    )

    # Check that `stan` function arguments are called correctly
    expect_equal(
        as.StanModule(x, model = LongitudinalGSF()),
        StanModule("lm-gsf/link_ttg.stan")
    )

    expect_equal(
        getParameters(x),
        ParameterList(
            Parameter(prior = prior_normal(0, 5), name = "bob", size = 1)
        )
    )

    expect_equal(
        x@key,
        "bob"
    )


    # Resolving promise throws an error if the function changes the key
    x <- PromiseLinkComponent(
        fun = \(prior, model, ...) {
            LinkComponent(
                stan = StanModule("lm-gsf/link_ttg.stan"),
                prior = prior,
                key = "steve"
            )
        },
        key = "bob",
        prior = prior_normal(0, 5)
    )
    expect_error(
        resolvePromise(x, model = LongitudinalGSF()),
        regex = "the same key as the promise"
    )
})
