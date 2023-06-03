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
