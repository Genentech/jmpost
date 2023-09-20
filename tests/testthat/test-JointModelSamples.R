
mcmc_results <- get_mcmc_results()

# longitudinal ----

test_that("longitudinal works as expected for JointModelSamples", {
    expect_s4_class(mcmc_results, "JointModelSamples")
    result <- longitudinal(mcmc_results)
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, as.list(mcmc_results@data)$Nind)
    expect_true(is.list(result))
    one_result <- result[[3]]
    expect_type(one_result, "list")
    expect_type(one_result$samples, "double")
    expect_identical(dim(one_result$samples), c(100L, 201L))
    expect_s3_class(one_result$observed, "data.frame")
    expect_identical(colnames(one_result$observed), c("t", "y", "median", "lower", "upper"))
    expect_s3_class(one_result$summary, "data.frame")
    expect_identical(colnames(one_result$summary), c("time", "median", "lower", "upper"))
    expect_identical(nrow(one_result$summary), 201L)
})


test_that("longitudinal allows to subset patients and times", {
    patients <- c("pt_00001", "pt_00005", "pt_00010", "pt_00022")
    time_grid <- c(1, 40, 100)
    result <- longitudinal(mcmc_results, patients = patients, time_grid = time_grid)
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, length(patients))
    expect_identical(names(result), patients)

    one_result <- result[[3]]
    expect_identical(dim(one_result$samples), c(100L, length(time_grid)))
    expect_identical(nrow(one_result$summary), length(time_grid))
})
