

test_that("Longitudinal Model doesn't print sampler rejection messages", {
    # These rejections typically happen when the sampler samples a
    # 0 value for the variance parameter. Sensible initial values +
    # setting near 0 limits (as opposed to 0) should avoid this
    ensure_test_data_1()

    mp <- capture_messages({
        devnull_out <- capture.output({
            devnull_model <- sampleStanModel(
                test_data_1$jmodel,
                data = test_data_1$jdata,
                iter_sampling = 3,
                iter_warmup = 3,
                chains = 1,
                refresh = 0,
                parallel_chains = 1
            )
        })
    })

    expect_false(any(grepl("The current Metropolis proposal is about to be rejected", mp)))
})
