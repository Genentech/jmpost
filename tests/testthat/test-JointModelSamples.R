

test_that("print works as expected for JointModelSamples", {
    ensure_test_data_1()
    expect_snapshot({
        devnull <- capture.output({
            test_data_1$jdata
        })
        print(mpp)
    })
})
