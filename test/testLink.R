


testthat::test_that("LinkNone object is generated without unexpected input", {
    expect_error(
        LinkNone(stan = StanModule(), parameters = ParameterList(mu = 0.2)),
        "unused argument",
        ignore.case = TRUE
    )
    checkmate::expect_class(LinkNone()@stan, "StanModule Object")
})



