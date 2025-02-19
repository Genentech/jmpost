
test_data_1 <- ensure_test_data_1()

test_that("print works as expected for JointModelSamples", {
    expect_snapshot({
        print(test_data_1$jsamples)
    })
})



test_that("saving and restoring samples from disk works as expected", {
    samps <- test_data_1$jsamples

    tfile <- tempfile(fileext = ".Rds")
    saveObject(samps, file = tfile)

    samps2 <- readRDS(tfile)

    # Can't compare entire object as some components contain formulas
    # whose environment component will be different no matter what
    expect_equal(
        samps@data@survival@data,
        samps2@data@survival@data
    )
    expect_equal(
        samps@data@longitudinal@data,
        samps2@data@longitudinal@data
    )
    # Key bit is that the retieved samples are identical
    expect_equal(
        posterior::as_draws_df(samps@results),
        posterior::as_draws_df(samps2@results)
    )
})
