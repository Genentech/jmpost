
run_stan_function <- function(stan_data, fun_files, dir, basename) {
    stan_mods <- lapply(fun_files, \(i) StanModule(i))
    stan_mod <- Reduce(merge, stan_mods)
    stan_plus_user <- merge(
        stan_mod,
        StanModule(test_path("models", "null_model_insert.stan"))
    )
    stan_data$null_y_data <- c(0, 1)
    mod <- cmdstanr::cmdstan_model(
        stan_file = cmdstanr::write_stan_file(
            as.character(stan_plus_user),
            dir = dir,
            basename = paste0("testfuns_", basename, ".stan")
        ),
        exe_file = file.path(
            dir,
            paste0(
                "testfuns_",
                basename,
                if (is_windows()) ".exe" else ""
            )
        )
    )

    # Completely silence call to STAN as we aren't fitting a real model
    devnull <- capture.output({
        suppressMessages({
            suppressWarnings({
                fit <- mod$sample(
                    data = stan_data,
                    refresh = 0,
                    chains = 1,
                    iter_sampling = 1,
                    iter_warmup = 1,
                    show_messages = FALSE
                )
            })
        })
    })
    return(fit)
    mod$stan_file()
}




test_that("GSF SLD function works as expected", {
    stan_data <- list(
        sld_time = c(0, 0.5, 1, 2),
        sld_bsld = c(50, 40, 40, 50),
        sld_s = c(0.1, 0.2, 0.3, 0.4),
        sld_g = c(0.2, 0.3, 0.2, 0.3),
        sld_phi = c(0.5, 0.4, 0.5, 0.3)
    )
    stan_data["sld_n"] <- length(stan_data$sld_time)

    fit <- run_stan_function(
        stan_data,
        c(
            "base/functions.stan",
            "lm-gsf/functions.stan",
            test_path("models", "gsf_sld.stan")
        ),
        CACHE_DIR,
        "gsf-sld"
    )

    observed <- as.numeric(fit$draws(variables = "sld_results")) |> round(4)

    expected <- gsf_sld(
        stan_data$sld_time,
        stan_data$sld_bsld,
        stan_data$sld_s,
        stan_data$sld_g,
        stan_data$sld_phi
    ) |>
        round(4)

    expect_equal(observed, expected)
})





test_that("Normal Log Density functions work as expected", {
    stan_data <- list(
        nrm_logden_obs   = c(5, 4, 3, 9, 10),
        nrm_logden_mu    = c(4, 4, 3, 3, 10),
        nrm_logden_sigma = c(2, 3, 4, 5,  6),
        nrm_logcum_quant  = 5,
        nrm_logcum_mu    = c(4, 4, 3, 3, 10),
        nrm_logcum_sigma = c(2, 3, 4, 5,  6)
    )
    stan_data["nrm_logden_n"] <- length(stan_data$nrm_logden_mu)
    stan_data["nrm_logcum_n"] <- length(stan_data$nrm_logcum_mu)

    fit <- run_stan_function(
        stan_data,
        c(
            "base/functions.stan",
            "lm-gsf/functions.stan",
            test_path("models/normal_log_cumulative.stan")
        ),
        CACHE_DIR,
        "log-cumulative"
    )

    #### vect_normal_log_dens
    observed <- as.numeric(fit$draws(variables = "nrm_logden_results")) |> round(5)

    expected <- dnorm(
        stan_data$nrm_logden_obs,
        stan_data$nrm_logden_mu,
        stan_data$nrm_logden_sigma,
        log = TRUE
    ) |>
        round(5)

    expect_equal(observed, expected)


    ### vect_normal_log_cum
    observed <- as.numeric(fit$draws(variables = "nrm_logcum_results")) |> round(5)

    expected <- pnorm(
        stan_data$nrm_logcum_quant,
        stan_data$nrm_logcum_mu,
        stan_data$nrm_logcum_sigma,
        log = TRUE
    ) |>
        round(5)

    expect_equal(observed, expected)
})



test_that("GSF Identity Link Function works as expected", {
    stan_data <- list(
        sld_time = matrix(
            c(
                0, 0.5, 1, 2,
                0, 0.2, 0.4, 0.6,
                0, 5, 2, 6
            ),
            byrow = TRUE,
            nrow = 3
        ),
        sld_bsld = c(50, 40, 40),
        sld_s = c(0.1, 0.2, 0.3),
        sld_g = c(0.2, 0.3, 0.2),
        sld_phi = c(0.5, 0.4, 0.5)
    )

    stan_data["sld_n"] <- nrow(stan_data$sld_time)
    stan_data["time_p"] <- ncol(stan_data$sld_time)


    fit <- run_stan_function(
        stan_data,
        c(
            "base/functions.stan",
            "lm-gsf/functions.stan",
            "lm-gsf/link_identity.stan",
            test_path("models", "gsf_identity_link.stan")
        ),
        CACHE_DIR,
        "gsf_identity_link"
    )

    observed <- as.numeric(fit$draws(variables = "results")) |> round(3)

    # Key thing we are testing is that the identity link loops over a matrix
    # We are testing to see if it is looping correctly
    expected <- c()
    for (i in seq_len(ncol(stan_data$sld_time))) {
        sld <- gsf_sld(
            stan_data$sld_time[, i],
            stan_data$sld_bsld,
            stan_data$sld_s,
            stan_data$sld_g,
            stan_data$sld_phi
        )
        expected <- c(expected, sld)
    }
    expected <-  expected |> round(3)

    expect_equal(observed, expected)
})
