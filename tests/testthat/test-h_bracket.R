


test_that("h_bracket works as expected", {
    st_mod <- StanModule(
        functions = "string",
        data = "string",
        parameters = "string",
        transformed_parameters = "string",
        generated_quantities = "string",
        priors = list("prior1" = "item1;","prior2" = "item2;")
    )

    expect_equal(h_bracket(st_mod@functions), "{\n    string\n}\n")
    expect_equal(h_bracket(st_mod@data), "{\n    string\n}\n")
    expect_equal(h_bracket(st_mod@parameters), "{\n    string\n}\n")
    expect_equal(h_bracket(st_mod@transformed_parameters), "{\n    string\n}\n")
    expect_equal(h_bracket(st_mod@generated_quantities), "{\n    string\n}\n")
})


test_that("Empty strings work fine", {
    str <- ""
    actual <- h_bracket(str)
    expected <- NULL
    expect_equal(actual, expected)
})
