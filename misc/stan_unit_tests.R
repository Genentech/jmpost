library(cmdstanr)
library(testthat)

devtools::document()
devtools::load_all()


NULL_MODEL_INSERT <- "
data {
    real y_mean;
}

parameters {
    real y;
}

model {
    y ~ normal(y_mean, 1);
}
"

run_stan_function <- function(stan_insert, stan_data, fun_files) {
    stan_mods <- lapply(fun_files, \(i) StanModule(i))
    stan_mod <- Reduce(merge, stan_mods)
    stan_plus_user <- merge(
        stan_mod,
        StanModule(stan_insert)
    )
    stan_data$y_mean <- 0
    stan_final <- merge(
        stan_plus_user,
        StanModule(NULL_MODEL_INSERT)
    )
    mod <- cmdstan_model(write_stan_file(as.character(stan_final)))
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
}



sld_insert <- "
data {
    int sld_n;
    vector[sld_n] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;
}


generated quantities {
    vector[sld_n] sld_results = sld(sld_time, sld_bsld, sld_s, sld_g, sld_phi);
}
"

sld_data <- list(
    sld_time = c(0, 0.5, 1, 2),
    sld_bsld = c(50, 40, 40, 50),
    sld_s = c(0.1, 0.2, 0.3, 0.4),
    sld_g = c(0.2, 0.3, 0.2, 0.3),
    sld_phi = c(0.5, 0.4, 0.5, 0.3)
)
sld_data["sld_n"] <- length(sld_data$sld_time)

fit <- run_stan_function(
    sld_insert,
    stan_data, 
    c("base/functions.stan", "lm-gsf/functions.stan")
)


observed <- as.numeric(fit$draws(variables = "sld_results")) |>
    round(4)



sld <- function(t, b , s, g, p){
    b * (p * exp(-s * t) + (1 - p) * exp(g * t))
}

expected <- sld(
    sld_data$sld_time,
    sld_data$sld_bsld,
    sld_data$sld_s,
    sld_data$sld_g,
    sld_data$sld_phi
) |>
    round(4)

expect_equal(observed, expected)












