





test_that("extract_survival_quantities() works as expected", {
    log_surv <- log(c(0.1, 0.3, 0.5, 0.7, 0.2))
    log_haz <- log(c(10, 11, 4, 2.1, 9, 3))


    nullmodel <- StanModule(test_path("models", "null_model_insert.stan"))
    stan_code <- sprintf("
generated quantities {
    vector[%i] log_surv_fit_at_time_grid;
    vector[%i] log_haz_fit_at_time_grid;
    log_surv_fit_at_time_grid = to_vector({%s});
    log_haz_fit_at_time_grid = to_vector({%s});
}",
        length(log_surv),
        length(log_haz),
        paste0(log_surv, collapse = ","),
        paste0(log_haz, collapse = ",")
    )

    gq_code <- StanModule(stan_code)

    model_code <- merge(
        gq_code,
        nullmodel
    )

    mod <- cmdstanr::cmdstan_model(
        stan_file = cmdstanr::write_stan_file(
            as.character(model_code),
            dir = CACHE_DIR,
            basename = "test_extract_gq.stan"
        ),
        exe_file = file.path(
            CACHE_DIR,
            paste0("test_extract_gq")
        )
    )

    ## Mock data just to get function to run
    stan_data <- list(null_y_data = c(0, 1))
    stan_fitted <- posterior::as_draws_matrix(list(null_y_mean = c(1)))

    gq <- mod$generate_quantities(
        data = stan_data,
        fitted_params = stan_fitted
    )


    run_test <- function(vals, keyword) {
        expected <- posterior::as_draws_matrix(matrix(vals, nrow = 1))
        colnames(expected) <- sprintf("quantity[%i]", seq_along(vals))
        expect_equal(
            round(extract_survival_quantities(gq, type = keyword), 3),
            round(expected, 3)
        )
    }

    run_test(exp(log_surv), "surv")
    run_test(-log_surv, "cumhaz")
    run_test(exp(log_haz), "haz")
    run_test(log_haz, "loghaz")

})
