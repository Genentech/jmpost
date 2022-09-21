test_that("read_stan is working as expected with characters", {
    actual <- read_stan("This is a character!")
    expected <- "This is a character!"
    expect_equal(actual, expected)
})
