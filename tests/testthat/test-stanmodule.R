



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
            "functions {",
            "    some code",
            "    some more code",
            "}",
            "data {",
            "    some data",
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




test_that("as.character works for the list objects", {
    obj <- StanModule(priors = list(
        "prior1" = "def1;",
        "prior2" = "def2;"
    ))
    actual <- as.character(obj)

    expected <- "model {\n    prior1 ~ def1;\n    prior2 ~ def2;\n    \n}\n"

    expect_equal(actual, expected)
})




test_that("loading multiple lines from a file works as expected", {
    temp_1 <- tempfile()
    temp_2 <- tempfile()

    sink(temp_1)
    cat("mystring; mystring2\nmystring3;\nmystring4;\n")
    sink()

    sink(temp_2)
    cat("more strings\n")
    sink()

    actual <- StanModule(
        generated_quantities = c(temp_1, temp_2)
    )

    expect_equal(
        actual@generated_quantities,
        c("mystring; mystring2\nmystring3;\nmystring4;", "more strings")
    )

    actual_char <- as.character(actual)
    expected <- paste0(
        c(
           "generated quantities {",
            "    mystring; mystring2",
            "    mystring3;",
            "    mystring4;",
            "    more strings",
            "}",
            ""
        ),
        collapse = "\n"
    )
    expect_equal(actual_char, expected)
})




test_that("is_file can correctly detect files", {
    dir1 <- tempfile()
    file1 <- tempfile()

    dir.create(dir1)
    file.create(file1)

    expect_error(is_file(NA), "`filename` must be")
    expect_error(is_file(123), "`filename` must be")
    expect_false(is_file(NA_character_))
    expect_false(is_file(NULL))
    expect_false(is_file(""))
    expect_false(is_file("random string"))
    expect_false(is_file(dir1))
    expect_true(is_file(file1))
})



test_that("StanModule errors if priors aren't named", {

    # Basic case when everything is specified correctly
    sm <- StanModule(priors = list("a" = "x", "b" = "y"))
    expect_equal(as.character(sm), "model {\n    a ~ x\n    b ~ y\n    \n}\n")


    # Case when no priors have names
    expect_error(
        StanModule(priors = list("x", "y")),
        regexp = "`Priors` must have names"
    )

    # Case when no only some have names
    expect_error(
        StanModule(priors = list("a" = "x", "y")),
        regexp = "`Priors` must have names"
    )
})


test_that("StanModule as.character works well with model and prior being specified", {
    actual <- as.character(StanModule(model = "b"))
    expected <- paste0(
        c(
            "model {",
            "    b",
            "}",
            ""
        ),
        collapse = "\n"
    )
    expect_equal(actual, expected)


    actual <- as.character(StanModule(priors = list("b" = "z")))
    expected <- paste0(
        c(
            "model {",
            "    b ~ z",
            "    ",
            "}",
            ""
        ),
        collapse = "\n"
    )
    expect_equal(actual, expected)


    actual <- as.character(StanModule(priors = list("b" = "z"), model = "mod"))
    expected <- paste0(
        c(
            "model {",
            "    b ~ z",
            "    mod",
            "}",
            ""
        ),
        collapse = "\n"
    )
    expect_equal(actual, expected)
})
