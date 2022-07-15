



test_that("Simple construction works as expected", {
    
    actual <- StanModule(
        functions = "string",
        data = c("string1", "string2")
    )
    
    expect_equal(actual@functions, "string")
    expect_equal(actual@data, c("string1", "string2"))
    expect_equal(actual@parameters, "")
    expect_equal(actual@priors, list())
})



test_that("Loading from file works as expected", {
    temp_1 <- tempfile()
    temp_2 <- tempfile()
    
    sink(temp_1)
    cat("mystring; mystring2\n")
    sink()
    
    sink(temp_2)
    cat("more strings\n")
    sink()
    
    actual <- StanModule(
        generated_quantities = c(temp_1, temp_2)
    )
    
    expect_equal(actual@generated_quantities, c("mystring; mystring2", "more strings"))
})



test_that("StanModules can be rendered as a string", {
    
    x <- StanModule(
        data = c("some data"),
        functions = c("some code", "some more code")
    )
    
    expected <- paste0(
        c(
            "",
            "functions {",
            "some code",
            "some more code",
            "}",
            "",
            "data {",
            "some data",
            "}",
            ""
        ),
        collapse = "\n"
    )
    
    actual <- as.character(x)
    
    expect_equal(actual, expected)
})



test_that("We are able to merge 2 StanModule objects together", {
    
    x <- StanModule(
        functions = c("abc", "def"),
        data = c("xyz")
    )
    
    y <- StanModule(
        functions = "hello",
        generated_quantities = "bye"
    )
    
    actual <- merge(x, y)
    expected <- StanModule(
        functions = c("abc", "def", "hello"),
        data = "xyz",
        generated_quantities = "bye"
    )
    
    expect_equal(actual, expected)
})



test_that("remove_blank_string works as expected", {
    
    actual <- remove_blank_strings(c(""))
    expected <- ""
    expect_equal(actual, expected)
    
    
    actual <- remove_blank_strings(c("", ""))
    expected <- ""
    expect_equal(actual, expected)
    
    
    actual <- remove_blank_strings(c("", "", "string"))
    expected <- "string"
    expect_equal(actual, expected)
    
    
    actual <- remove_blank_strings(c("string", "", "", "st"))
    expected <- c("string", "st")
    expect_equal(actual, expected)

})
