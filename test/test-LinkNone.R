


test_that("LinkNone object is generated without unexpected input", {
    expect_error(
        LinkNone(stan = StanModule(), parameters = ParameterList(mu = 0.2)),
        "unused argument",
        ignore.case = TRUE
    )
    
    link <- LinkNone()
    expect_true(is(link, "Link"))
    expect_true(is(link, "LinkNone"))
    expect_false(is(link, "LongitudinalModel"))
    expect_false(is(link, "link_gsf_abstract"))
})




