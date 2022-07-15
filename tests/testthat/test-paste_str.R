test_that("Basic functionality paste_str works as expected", {

    actual <- paste_str(c("bla", "bla2"))
    expected <- "bla;\\n bla2;\\n"
    expect_equal(actual, expected)


    actual <- paste_str(c("bla;", "bla2;"))
    expected <- "bla;\\n bla2;\\n"
    expect_equal(actual, expected)


    actual <- paste_str(c("bla;", "bla2;", "gfdaf;"))
    expect_equal(length(actual), 1)


    string <- c(
        "real beta;\\n real sigma",
    )
    actual <- paste_str(c("real beta;\\n real sigma"))
    expected <- "real beta;\\n real sigma;\\n"
    expect_equal(actual, expected)


    string <- c(
        "real beta;\\n real sigma",
        "real gamma"
    )
    actual <- paste_str(string)
    expected <- "real beta;\\n real sigma;\\n real gamma;\\n"
    expect_equal(actual, expected)
})

