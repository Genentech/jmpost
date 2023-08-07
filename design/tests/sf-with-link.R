library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = FALSE)



#### Example 1 - Fully specified model - using the defaults for everything



## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(300),
    times = seq(0, 900),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_sf(
       sigma = 0.025,
        mu_s = c(0.007),
        mu_g = c(0.001),
        mu_b = 60,
        omega_b = 0.51,
        omega_s = 0.51,
        omega_g = 0.51,
        link_dsld = 0.002,
        link_ttg = -0.002
    ),
    os_fun = sim_os_exponential(
        lambda = 1/400
    )
)


## Extract data to individual datasets
dat_os <- jlist$os

select_times <- seq(0, 600, by = 50)

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(pt, time)




pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col =pt, group =pt)) +
    geom_line(aes(x = time, y = sld, col =pt, group =pt)) +
    theme_bw()


jm <- JointModel(
    longitudinal = LongitudinalSF(

        mu_bsld = prior_lognormal(log(60), 0.5),
        mu_ks = prior_lognormal(log(0.007), 0.5),
        mu_kg = prior_lognormal(log(0.001), 0.5),

        omega_bsld = prior_lognormal(log(0.51), 0.5),
        omega_ks = prior_lognormal(log(0.51), 0.5),
        omega_kg = prior_lognormal(log(0.51), 0.5),

        sigma = prior_lognormal(log(0.01), 1)
    ),
    survival = SurvivalExponential(
        lambda = prior_gamma(2, 5),
        beta = prior_normal(1, 1)
    ),
    link = LinkSF(
        link_sf_dsld(beta = prior_normal(1, 0.5)),
        link_sf_ttg(gamma = prior_normal(1, 0.5))
    )
)


# Chain 1 ---Ythreshold--- 
# Chain 1 5 
# Chain 1 ---Ypred[cens_y_index]--- 
# Chain 1 [33.2841,31.2086,31.6244,33.8215,37.3285] 
# Chain 1 ---lm_sf_sigma--- 
# Chain 1 0.0164872 
# Chain 1 ---Nta_cens_y--- 
# Chain 1 5 

# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
jdat <- DataJoint(
    survival = DataSurvival(
        data = dat_os,
        formula = Surv(time, event) ~ cov_cat + cov_cont,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        subject = "pt",
        threshold = 5
    )
)

## Sample from JointModel

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 1,
    iter_warmup = 1,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)





vars <- c(
    "lm_gsf_mu_bsld[1]",
    "lm_gsf_mu_phi[1]", "lm_gsf_mu_phi[2]",
    "lm_gsf_mu_kg[1]", "lm_gsf_mu_kg[2]",
    "lm_gsf_mu_ks[1]", "lm_gsf_mu_ks[2]",
    "lm_gsf_sigma",
    "lm_gsf_omega_bsld", "lm_gsf_omega_kg",
    "lm_gsf_omega_phi", "lm_gsf_omega_ks",
    "sm_exp_lambda", "lm_gsf_beta", "lm_gsf_gamma"
)



################################
#
# General Diagnostic stuff
#
#



mp$summary(vars)
library(bayesplot)
mcmc_trace(mp$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp$draws(), vars)
