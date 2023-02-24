library(dplyr)
library(ggplot2)
library(stringr)
library(survival)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = FALSE)



#### Example 1 - Fully specified model - using the defaults for everything



## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(30, 40),
    max_time = 2000,
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.01,
        mu_s = c(3, 4),
        mu_g = c(0.2, 0.3),
        mu_phi = c(0.1, 0.2),
        mu_b = c(50, 60),
        eta_b_sigma = 5,
        eta_s_sigma = 2,
        eta_g_sigma = 1,
        eta_phi_sigma = 5,
        omega_b = 0.135,
        omega_s = 0.15,
        omega_g = 0.225,
        omega_phi = 0.75,
    ),
    os_fun = sim_os_weibull(
        lambda = 0.00333,
        gamma = 0.97
    )
)




## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(50, 50),
    max_time = 2000,
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.01,
        mu_s = c(0.15, 0.25),
        mu_g = c(0.15, 0.25),
        mu_phi = c(0.4, 0.6),
        mu_b = c(50, 55),
        eta_b_sigma = 5,
        eta_s_sigma = 2,
        eta_g_sigma = 1,
        eta_phi_sigma = 5,
        omega_b = 0.135,
        omega_s = 0.15,
        omega_g = 0.225,
        omega_phi = 0.75,
    ),
    os_fun = sim_os_weibull(
        lambda = 0.00333,
        gamma = 0.97
    )
)


## Extract data to individual datasets
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    #dplyr::filter(time %in% c(seq(1, 2000, by = 30))) |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)

# mean(dat_os$time)
# mean(dat_os$event)

library(dplyr)
library(ggplot2)
pdat <- dat_lm |>
    dplyr::filter(pt == "pt_00011")

ggplot(data = pdat, aes(x = time, y = sld)) +
    geom_point() +
    geom_line() +
    theme_bw()


devtools::document()
devtools::load_all()
jm <- JointModel(
    longitudinal_model = LongitudinalGSF(
        
        mu_bsld = Parameter(prior_lognormal(log(50), 5), init = log(50)),
        mu_ks = Parameter(prior_lognormal(log(0.2), 0.5), init = log(3)),
        mu_kg = Parameter(prior_lognormal( log(0.2), 1), init = log(0.2)),
        mu_phi = Parameter(prior_beta(2, 2), init = 0.2),
        
        omega_bsld = Parameter(prior_lognormal(log(0.135), 1), init = 0),
        omega_ks = Parameter(prior_lognormal(log(0.15), 1), init = 0),
        omega_kg = Parameter(prior_lognormal(log(0.225), 1), init = 0),
        omega_phi = Parameter(prior_lognormal(log(0.75), 1), init = 0),
        
        sigma = Parameter(prior_lognormal(log(0.01), 1), init = 0),
        
        tilde_bsld = Parameter(prior_normal(0, 5), init = 0),
        tilde_ks = Parameter(prior_normal(0, 2), init = 0),
        tilde_kg = Parameter(prior_normal(0, 1), init = 0),
        tilde_phi = Parameter(prior_normal(0, 5), init = 0)
    )
)

jm <- JointModel(
    longitudinal_model = LongitudinalGSF(),
    link = LinkGSF()
)

link_gsf_dsld()


# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
stan_data <- as_stan_data(dat_os, dat_lm, ~ cov_cat + cov_cont)


## Sample from JointModel

dir.create(path = file.path("local"), showWarnings = FALSE)
mp <- sampleStanModel(
    jm,
    data = stan_data,
    iter_sampling = 500,
    iter_warmup = 1000,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)


mcmc_trace(mp$draws("lm_gsf_mu_phi[1]"))

### Example of how to compile model without running it
# model <- compileStanModel(jm, file.path("local", "full_stan"))


### Extract parameters and calculate confidence intervals
draws_means <- mp$draws(format = "df") |>
    gather() |>
    group_by(key) |>
    summarise(
        mean = mean(value),
        sd = sd(value),
        lci = quantile(value, 0.025),
        uci = quantile(value, 0.975)
     )


#### Get a list of all named parameters
# draws_means$key |> str_replace("\\[.*\\]", "[]") |> unique()


draws_means |> filter(key == "log_lik[42]")


vars <- c(
    "lm_gsf_mu_bsld[1]", "lm_gsf_mu_bsld[2]",
    "lm_gsf_mu_phi[1]", "lm_gsf_mu_phi[2]",
    "lm_gsf_mu_kg[1]", "lm_gsf_mu_kg[2]",
    "lm_gsf_mu_ks[1]", "lm_gsf_mu_ks[2]",
    "lm_gsf_sigma",
    "lm_gsf_omega_bsld", "lm_gsf_omega_kg",
    "lm_gsf_omega_phi", "lm_gsf_omega_ks"
)

draws_means |>
    filter(key %in% vars) |>
    mutate(key = factor(key, levels = vars)) |>
    arrange(key)







mp$expose_functions()
#> Compiling standalone functions...
mp$functions$rtnx(5)



install.packages("decor")

library(bayesplot)

mcmc_hist(mp$draws("lm_gsf_mu_phi[1]"))











library(cmdstanr)
model <- "functions {
    
    // Vectorized ifelse() similar as in R.
    row_vector ifelse(array[] int condition, row_vector yes, row_vector no) {
        row_vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }
    vector ifelse(array[] int condition, vector yes, vector no) {
        return ifelse(condition, yes', no')';
    }
    
    array[] int is_negative(row_vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }
    
    array[] int is_negative(vector x) {
        return is_negative(x');
    }
    
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        vector[num_elements(time)] psi_phi_mod = ifelse(
            is_negative(time),
            zeros_vector(num_elements(time)),
            psi_phi
        );
        vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }
    

    
}
data { real y_mean; } parameters { real y; } model { y ~normal(y_mean, 1); }"
mod <- cmdstan_model(write_stan_file(model))
fit <- mod$sample(data = list(y_mean = 0), refresh = 0)


fit$expose_functions()

fit$functions$rtnx(5)



model <- "functions { real rtnx(real x) { return x; } } data { real y_mean; } parameters { real y; } model { y ~normal(y_mean, 1); }"
mod <- cmdstan_model(write_stan_file(model))
fit <- mod$sample(data = list(y_mean = 0), refresh = 0)
fit$expose_functions()
fit$functions$rtnx(5)







library(cmdstanr)
model <- "functions {
    vector ifelse(array[] int condition, vector yes, vector no) {
        vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }
    
    array[] int is_negative(vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }
    
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        vector[num_elements(time)] psi_phi_mod = ifelse(
            is_negative(time),
            zeros_vector(num_elements(time)),
            psi_phi
        );
        vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }
}
data { real y_mean; } parameters { real y; } model { y ~normal(y_mean, 1); }"
mod <- cmdstan_model(write_stan_file(model))
fit <- mod$sample(data = list(y_mean = 0), refresh = 0)
fit$expose_functions()



fit$functions$sld(1, 50, 1, 2, 0.5)


sld <- function(time, bsld, s, g, phi) {
    
}




