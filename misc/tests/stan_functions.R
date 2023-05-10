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



stan_insert <- "
data {
    int sld_n;
    vector[sld_n] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;

    int nrm_logden_n;
    vector[nrm_logden_n] nrm_logden_obs;
    vector[nrm_logden_n] nrm_logden_mu;
    vector[nrm_logden_n] nrm_logden_sigma;

    int nrm_logcum_n;
    real nrm_logcum_quant;
    vector[nrm_logcum_n] nrm_logcum_mu;
    vector[nrm_logcum_n] nrm_logcum_sigma;
}


generated quantities {
    vector[sld_n] sld_results = sld(sld_time, sld_bsld, sld_s, sld_g, sld_phi);

    vector[nrm_logden_n] nrm_logden_results = vect_normal_log_dens(
        nrm_logden_obs,
        nrm_logden_mu,
        nrm_logden_sigma
    );

    vector[nrm_logcum_n] nrm_logcum_results = vect_normal_log_cum(
        nrm_logcum_quant,
        nrm_logcum_mu,
        nrm_logcum_sigma
    );
}
"

stan_data <- list(
  sld_time = c(0, 0.5, 1, 2),
  sld_bsld = c(50, 40, 40, 50),
  sld_s = c(0.1, 0.2, 0.3, 0.4),
  sld_g = c(0.2, 0.3, 0.2, 0.3),
  sld_phi = c(0.5, 0.4, 0.5, 0.3),
  nrm_logden_obs = c(5, 4, 3, 9, 10),
  nrm_logden_mu = c(4, 4, 3, 3, 10),
  nrm_logden_sigma = c(2, 3, 4, 5, 6),
  nrm_logcum_quant = 5,
  nrm_logcum_mu = c(4, 4, 3, 3, 10),
  nrm_logcum_sigma = c(2, 3, 4, 5, 6)
)

stan_data["sld_n"] <- length(stan_data$sld_time)
stan_data["nrm_logden_n"] <- length(stan_data$nrm_logden_mu)
stan_data["nrm_logcum_n"] <- length(stan_data$nrm_logcum_mu)

fit <- run_stan_function(
  stan_insert,
  stan_data,
  c("base/functions.stan", "lm-gsf/functions.stan")
)




#### SLD

observed <- as.numeric(fit$draws(variables = "sld_results")) |> round(4)

expected <- sld(
  stan_data$sld_time,
  stan_data$sld_bsld,
  stan_data$sld_s,
  stan_data$sld_g,
  stan_data$sld_phi
) |>
  round(4)

expect_equal(observed, expected)





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
