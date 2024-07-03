
test_data_1 <- ensure_test_data_1()

test_that("print works as expected for JointModelSamples", {
    expect_snapshot({
        print(test_data_1$jsamples)
    })
})
