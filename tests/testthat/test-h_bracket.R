


test_that("h_bracket works as expected", {
    st_mod <- StanModule(
        functions = "string",
        data = "string",
        parameters = "string",
        transformed_parameters = "string",
        generated_quantities = "string",
        priors = list("prior1" = "item1;","prior2" = "item2;")
    )

    expect_equal(h_bracket(st_mod@functions), "{\nstring\n}\n")
    expect_equal(h_bracket(st_mod@data), "{\nstring\n}\n")
    expect_equal(h_bracket(st_mod@parameters), "{\nstring\n}\n")
    expect_equal(h_bracket(st_mod@transformed_parameters), "{\nstring\n}\n")
    expect_equal(h_bracket(st_mod@generated_quantities), "{\nstring\n}\n")
})
