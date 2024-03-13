

test_that("print works as expected for JointModelSamples", {
    ensure_test_data_1()
    expect_snapshot({
        print(test_data_1$jsamples)
    })
})
