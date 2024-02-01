
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

    expect_snapshot({
        code <- paste(c(
            "data {\n    int x;\n}",
            "parameters {\n    real sigma;\n    real mu;  real par;\n}"
        ), collapse = "\n")
        x <- StanModule(code)
        print(x)
    })

    expect_snapshot({
        code <- paste(c(
            "data {\n    int x;\n}",
            "model {\n    real sigma;\n    real mu; \n}",
            "generated quantities {\n    //something \n}"
        ), collapse = "\n")
        x <- StanModule(code)
        print(x)
    })
})


test_that("StanModule() can read basic stan programs", {
    x <- StanModule(test_path("models", "stanmodule_1.stan"))
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@generated_quantities, "")
    expect_equal(x@functions, "")
    expect_equal(x@data, c("    int n;", "    array[n] real x;"))
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, c("   real mu;", "   real sigma;"))
    expect_equal(x@model, "   x ~ normal(mu, sigma);")
})



test_that("StanModule() can handle trailing } characters", {
    code <- "
data {
    int x;        
    int z;     }       
model {
    int z;
    int y;
}"
    x <- StanModule(code)
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@generated_quantities, "")
    expect_equal(x@functions, "")
    expect_equal(x@data, c("    int x;        ", "    int z;"))
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, "")
    expect_equal(x@model, c("    int z;", "    int y;"))
})




test_that("StanModule() works with in-line code", {
    code <- "
data {
    int x;
}
generated quantities {
    int z;
    // some line comment
}
transformed parameters {
    /*
    a block comment
    */
}"
    x <- StanModule(code)
    expect_equal(x@transformed_parameters, c("    /*", "    a block comment", "    */"))
    expect_equal(x@generated_quantities, c("    int z;", "    // some line comment"))
    expect_equal(x@functions, "")
    expect_equal(x@data, "    int x;")
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, "")
    expect_equal(x@model, "")
})



test_that("StanModule() throws an error for 1-line blocks", {
    code <- "
data { int x;}
model {
    int z;
    int y;
}"
    expect_error(StanModule(code), "`data`")


    code <- "
data {
    int x;}
model {
    int z;
    int y;
}
generated quantities { int z;}"
    expect_error(StanModule(code), "`generated quantities`")
})


test_that("StanModule() throws an error for code after the closing bracket", {
    code <- "
data { 
    int x;
}  Some follow up stuff
model {
    int z;
    int y;
}"
    expect_error(StanModule(code), "`data`")
})


test_that("StanModule() can load vignette example code", {
    code <- "
data {
  int n; array[n] real x;
}       
parameters{      
  real mu; 
  real sigma;}

    model {    
x ~ normal(mu, sigma);
    }    "
    x <- StanModule(code)
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@generated_quantities, "")
    expect_equal(x@functions, "")
    expect_equal(x@data, "  int n; array[n] real x;")
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, c("  real mu; ", "  real sigma;"))
    expect_equal(x@model, "x ~ normal(mu, sigma);")
})


testthat("StanModule.merge works as expected", {
    x <- merge(
        StanModule(),
        StanModule()
    )
    expect_equal(x@data, "")
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, "")
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@model, "")
    expect_equal(x@generated_quantities, "")
    expect_equal(x@functions, "")


    x <- merge(
        StanModule("model {\n    int x;\n}"),
        StanModule("data {\n    int x;\n}")
    )
    expect_equal(x@data, "    int x;")
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, "")
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@model, "    int x;")
    expect_equal(x@generated_quantities, "")
    expect_equal(x@functions, "")


    x <- merge(
        StanModule("parameters {\n    int x;\n}\n generated quantities {\n    int w;\n}"),
        StanModule("generated quantities {\n    int z;\n}\n parameters {\n    int y;\n}")
    )
    expect_equal(x@data, "")
    expect_equal(x@transformed_data, "")
    expect_equal(x@parameters, c("    int x;", "    int y;"))
    expect_equal(x@transformed_parameters, "")
    expect_equal(x@model, "")
    expect_equal(x@generated_quantities, c("    int w;", "    int z;"))
    expect_equal(x@functions, "")
})
