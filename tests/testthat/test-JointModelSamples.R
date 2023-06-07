# longitudinal ----

test_that("longitudinal works as expected for JointModelSamples", {
    expect_s4_class(mcmc_results, "JointModelSamples")
    result <- expect_silent(longitudinal(mcmc_results))
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, mcmc_results@data$Nind)
    expect_true(is.list(result))
    one_result <- result[[3]]
    expect_type(one_result, "list")
    expect_type(one_result$samples, "double")
    expect_identical(dim(one_result$samples), c(100L, mcmc_results@data$n_lm_time_grid))
    expect_s3_class(one_result$observed, "data.frame")
    expect_identical(colnames(one_result$observed), c("t", "y", "median", "lower", "upper"))
    expect_s3_class(one_result$summary, "data.frame")
    expect_identical(colnames(one_result$summary), c("time", "median", "lower", "upper"))
    expect_identical(nrow(one_result$summary), mcmc_results@data$n_lm_time_grid)
})

test_that("longitudinal allows to subset patients", {
    patients <- c("pt_00001", "pt_00005", "pt_00010", "pt_00022")
    result <- expect_silent(longitudinal(mcmc_results, patients = patients))
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, length(patients))
    expect_identical(names(result), patients)
})

# survival ----

test_that("survival works as expected for JointModelSamples", {
    expect_s4_class(mcmc_results, "JointModelSamples")
    result <- expect_silent(survival(mcmc_results))
    expect_s4_class(result, "SurvivalSamples")
    expect_length(result, mcmc_results@data$Nind)
    expect_true(is.list(result))
    one_result <- result[[3]]
    expect_type(one_result, "list")
    expect_type(one_result$samples, "double")
    expect_identical(dim(one_result$samples), c(100L, mcmc_results@data$n_sm_time_grid))
    expect_s3_class(one_result$observed, "data.frame")
    expect_identical(colnames(one_result$observed), c("t", "death", "median", "lower", "upper"))
    expect_s3_class(one_result$summary, "data.frame")
    expect_identical(colnames(one_result$summary), c("time", "median", "lower", "upper"))
    expect_identical(nrow(one_result$summary), mcmc_results@data$n_sm_time_grid)
})

test_that("survival allows to subset patients", {
    patients <- c("pt_00001", "pt_00005", "pt_00010", "pt_00022")
    result <- expect_silent(survival(mcmc_results, patients = patients))
    expect_s4_class(result, "SurvivalSamples")
    expect_length(result, length(patients))
    expect_identical(names(result), patients)
})
