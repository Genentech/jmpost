


testthat::test_that("each parameter of the LinkRandomSlope is placed and no relavent argument is detected", {
    linkobject <- LinkRandomSlope(link_lm_p = Parameter(init = 0.01))
    linkobject_error <- LinkRandomSlope(link_lm_phi = Parameter(prior_normal(0.1, 0.3), init = 0.05))


    expect_equal(linkobject@parameters@parameters[[1]]@init, 0.01)
    expect_equal(linkobject@parameters@parameters[[1]]@prior@parameters,
                 list())
    expect_equal(linkobject@parameters@parameters[[1]]@prior@"repr",
                 character(0))
    checkmate::expect_class(linkobject@stan@functions, "character")
    expect_error(
        LongitudinalRandomSlope(lm_rs_pp = Parameter(rist = 1)),
        "unused argument",ignore.case = TRUE)
})








