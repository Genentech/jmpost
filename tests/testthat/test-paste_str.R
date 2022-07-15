test_that("paste_str functionality", {

    test1 <- paste_str(vec =  c("bla", "bla2"))
    solution <- "bla;\\n bla2;\\n"
    expect_equal(test1, solution)

})

test_that("paste_str functionality 2", {

    test1 <- paste_str(vec =  c("bla;", "bla2;"))
    solution <- "bla;\\n bla2;\\n"
    expect_equal(test1, solution)

})

test_that("paste_str functionality 3", {

    test1 <- paste_str(vec =  c("bla;", "bla2;", "gfdaf;"))
    expect_equal(length(test1), 1)

})
