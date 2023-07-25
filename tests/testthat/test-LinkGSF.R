# LinkGSF-class ----

test_that("LinkGSF class initialization works as expected", {
    result <- expect_silent(.LinkGSF())
    expect_s4_class(result, "LinkGSF")
})

# LinkGSF-constructors ----

test_that("LinkGSF user constructor works as expected with defaults", {
    result <- expect_silent(LinkGSF())
    expect_s4_class(result, "LinkGSF")
})

# link_gsf_ttg-constructors ----

test_that("link_gsf_ttg user constructor works as expected with defaults", {
    result <- expect_silent(link_gsf_ttg())
    expect_s4_class(result, "link_gsf_ttg")
})

# link_gsf_dsld-constructors ----

test_that("link_gsf_dsld user constructor works as expected with defaults", {
    result <- expect_silent(link_gsf_dsld())
    expect_s4_class(result, "link_gsf_dsld")
})



test_that("LinkGSF returns correct defaults when no arguments are supplied", {

    get_par_names <- function(x) {
        vapply(x@parameters@parameters, function(x) x@name, character(1))
    }

    link <- LinkGSF()
    par_names <- get_par_names(link)
    expect_equal(par_names, c("lm_gsf_beta", "lm_gsf_gamma"))

    link <- LinkGSF(link_gsf_ttg())
    par_names <- get_par_names(link)
    expect_equal(par_names, c("lm_gsf_gamma"))
})
