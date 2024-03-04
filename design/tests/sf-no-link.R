library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

# devtools::document()
devtools::load_all(export_all = FALSE)

options("jmpost.cache_dir" = file.path("local", "models"))

#### Example 1 - Fully specified model - using the defaults for everything




## Generate Test data with known parameters
## (based on internal data, time in years)
# set.seed(7143)
# jlist <- simulate_joint_data(
#     .debug = TRUE,
#     n_arm = c(70, 90),
#     times = seq(0, 3, by = (1/365)/2),
#     lambda_cen = 1 / 9000,
#     beta_cat = c(
#         "A" = 0,
#         "B" = -0.1,
#         "C" = 0.5
#     ),
#     beta_cont = 0.3,
#     lm_fun = sim_lm_gsf(
#         sigma = 0.01,
#         mu_s = c(0.6),
#         mu_g = c(0.3),
#         mu_b = 60,
#         omega_b = 0.2,
#         omega_s = 0.2,
#         omega_g = 0.2,
#         a_phi = 6,
#         b_phi = 8
#     ),
#     os_fun = sim_os_exponential(
#         lambda = 1 / (400 / 365)
#     )
# )
set.seed(7143)
jlist <- simulate_joint_data(
    .debug = TRUE,
    design = list(
        SimGroup(85, "Arm-A", "Study-X"),
        SimGroup(100, "Arm-B", "Study-X")
    ),
    times = seq(0, 3, by = (1/365)/2),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_sf(),
    os_fun = sim_os_exponential(1 / (400 / 365))
)


set.seed(333)
select_times <- sample(jlist$lm$time, 12)




## Extract data to individual datasets
dat_os <- jlist$os


dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(time, pt) |>
    dplyr::mutate(time = time)

dat_lm |>
    dplyr::group_by(arm) |>
    dplyr::summarise(across(c("psi_b", "psi_g", "psi_s"), mean))


# mean(dat_os$time)
# mean(dat_os$event)


pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col =pt, group =pt)) +
    geom_line(aes(x = time, y = sld, col =pt, group =pt)) +
    theme_bw()

param <- dat_lm |>
    select(arm, psi_b, psi_s, psi_g) |>
    gather("KEY", "VAR", -arm)

ggplot(data = param, aes(x = VAR, group = arm, col = arm)) +
    geom_density() +
    theme_bw() +
    facet_wrap(~KEY, scales = "free")


jm <- JointModel(
    longitudinal = LongitudinalSteinFojo(
        mu_bsld = prior_normal(log(60), 1),
        mu_ks = prior_normal(log(0.6), 1),
        mu_kg = prior_normal(log(0.3), 1),
        omega_bsld = prior_lognormal(log(0.2), 1),
        omega_ks = prior_lognormal(log(0.2), 1),
        omega_kg = prior_lognormal(log(0.2), 1),
        sigma = prior_lognormal(log(0.01), 1),
        centred = TRUE
    ),
    survival = SurvivalExponential(
        lambda = prior_lognormal(log(1 / (400 / 365)), 1)
    )
)





# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
jdat <- DataJoint(
    subject = DataSubject(
        data = dat_os,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = dat_os,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        threshold = -999
    )
)


# jmpost:::initialValues(jm)


## Sample from JointModel

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_warmup = 600,
    iter_sampling = 1000,
    refresh = 200,
    chains = 3,
    parallel_chains = 3
)


summary_post <- function(model, vars, exp = FALSE) {
    dat <- model$summary(
        vars,
        mean = mean,
        q01 = \(x) purrr::set_names(quantile(x, 0.01), ""),
        q99 = \(x) purrr::set_names(quantile(x, 0.99), ""),
        rhat = posterior::rhat,
        ess_bulk = posterior::ess_bulk,
        ess_tail = posterior::ess_tail
    )
    if (exp) {
        dat$q01 <- dat$q01 |> exp()
        dat$q99 <- dat$q99 |> exp()
        dat$mean <- dat$mean |> exp()
    }
    dat
}

summary_post(mp@results, c("lm_sf_mu_bsld", "lm_sf_mu_kg", "lm_sf_mu_ks"), TRUE)
summary_post(mp@results, c("lm_sf_beta", "lm_sf_gamma"))




################################
#
# General Diagnostic stuff
#
#



library(bayesplot)
mcmc_trace(mp$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp$draws(), vars)

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


draws_means |>
    filter(key %in% vars) |>
    mutate(key = factor(key, levels = vars)) |>
    arrange(key)


