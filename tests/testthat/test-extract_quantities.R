

# This is potentially confusing so to help explain it
# The quantities matrix (inside stan not what is returned by stan) is
# 1 column per timepoint, 1 row per subject
# e.g.  quantity[3,5] = 3rd subject timepoint 5
# To account for the multiple sampling draws stan flatterns this e.g. stan returns a matrix
# of 1 column  per subject per timepoint  and 1 row per draw.
# Thus the column headers of the stan returned matrix are "quantity[3,5]" where the
# indexes refer to the matrix position within the internal stan matrix.
# ...
# Good luck :)


# To explain how the following unit test works though...
# the extract_quantities function essentially attempts to extract specific
# named quantities from the generated quantities block of our stan programs
# It then attemtps to reformat them as required for the designed quantity (e.g. log hazard)
# The unit test works by just spoofing those named quantities with a pre-derived
# matrix from within R.  E.g. we aren't actually computing the quantities we are just
# getting stan to return us a matrix that we passed to it in the first place.
# This way we can test to see that extract_quantities is (a) extracting the correct named
# element and (b) is formating it as expected

test_that("extract_quantities() works as expected", {
    set.seed(1910)
    log_surv <- matrix(log(runif(30)), nrow = 6)
    log_haz <- matrix(rnorm(40), nrow = 10)
    y_fit <- matrix(rnorm(50), nrow = 25)

    model_code <- StanModule(test_path("models", "extract_quantities.stan"))

    mod <- cmdstanr::cmdstan_model(
        stan_file = cmdstanr::write_stan_file(
            as.character(model_code),
            dir = CACHE_DIR,
            basename = "test_extract_gq.stan"
        ),
        exe_file = file.path(
            CACHE_DIR,
            paste0(
                "test_extract_gq",
                if (is_windows()) ".exe" else ""
            )
        )
    )

    ## Mock data just to get function to run
    stan_data <- list(
        null_y_data = c(0, 1),
        dim_surv = dim(log_surv),
        dim_haz = dim(log_haz),
        dim_y_fit = dim(y_fit),
        ret_log_surv = log_surv,
        ret_log_haz = log_haz,
        ret_y_fit = y_fit
    )
    stan_fitted <- posterior::as_draws_matrix(list(null_y_mean = 1:2))

    gq <- run_quietly({
        mod$generate_quantities(
            data = stan_data,
            fitted_params = stan_fitted
        )
    })

    run_test <- function(vals, keyword) {
        actual <- extract_quantities(gq, type = keyword)
        vals_vec <- as.vector(vals)
        raw_expected <- matrix(rep(vals_vec, 2), nrow = 2, byrow = TRUE)
        n <- nrow(vals)
        m <- ncol(vals)
        labels <- sprintf(
            "quantity[%i,%i]",
            rep(seq_len(n), times = m),
            rep(seq_len(m), each = n)
        )
        colnames(raw_expected) <- labels
        expected <- posterior::as_draws_matrix(raw_expected)
        expect_equal(
            round(actual, 3),
            round(expected, 3)
        )
    }

    run_test(exp(log_surv), "surv")
    run_test(-log_surv, "cumhaz")
    run_test(exp(log_haz), "haz")
    run_test(log_haz, "loghaz")
    run_test(y_fit, "lm_identity")
})
