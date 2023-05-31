test_that("subsetting works as expected for LongitudinalSamples", {
    mcmc_results <- readRDS(test_path("fixtures", "mcmc_results.rds"))
    object <- longitudinal(mcmc_results, patients = c("pt_00001", "pt_00022"))
    result <- object["pt_00001"]
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, 1L)
    expect_identical(names(result), "pt_00001")
})
