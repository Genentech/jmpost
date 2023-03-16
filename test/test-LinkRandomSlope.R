


test_that("each parameter of the LinkRandomSlope is placed and no relavent argument is detected", {
    
    linkobject <- LinkRandomSlope(
        link_lm_phi = Parameter(init = 0.01)
    )
    
    expect_true(is(linkobject@parameters, "ParameterList"))

    expect_equal(
        linkobject@parameters@parameters[[1]]@init,
        0.01
    )
    
    expect_equal(
        linkobject@parameters@parameters[[1]]@prior@parameters,
        list()
    )
    
    expect_equal(
        linkobject@parameters@parameters[[1]]@prior@repr,
        character(0)
    )
    
    expect_true(
        is(linkobject@stan, "StanModule")
    )
    
    expect_true(
        is.character(linkobject@stan@functions)
    )
    
    expect_true(
        length(linkobject@stan@functions) >= 1
    )
    
    expect_error(
        LongitudinalRandomSlope(lm_rs_pp = Parameter(rist = 1)),
        "unused argument",
        ignore.case = TRUE
    )
})








