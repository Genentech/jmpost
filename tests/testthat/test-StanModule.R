
test_that("StanModule print method works as expected", {
    expect_snapshot({
        code <- "functions{\n some fun \n}\n\ndata{ \n abd \n}"
        x <- StanModule(code)
        print(x)
    })

    expect_snapshot({
        code <- paste(c(
            "functions{\n some fun \n}\n",
            "model{ \n abd \n}\n",
            "generated quantities{ \n awdadawda \n}\n"
        ), collapse = "\n")
        x <- StanModule(code)
        print(x)
    })
})
