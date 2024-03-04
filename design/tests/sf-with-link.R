library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = TRUE)

options("jmpost.cache_dir" = file.path("local", "models"))



## Generate Test data with known parameters
jlist <- simulate_joint_data(
    design = list(
        SimGroup(110, "Arm-A", "Study-X"),
        SimGroup(110, "Arm-B", "Study-X")
    ),
    times = seq(0, 4, by = (1/365)/2),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_sf(
        sigma = 0.005,
        mu_s = c(0.2, 0.25),
        mu_g = c(0.15, 0.2),
        mu_b = 60,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1,
        link_ttg = -0.2,
        link_dsld = 0.2
    ),
    os_fun = sim_os_weibull(
        lambda = 365 * (1/400),
        gamma = 1
    )
)


## Extract data to individual datasets
dat_os <- jlist$os

select_times <- c(1, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900) * (1 / 365)

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(pt, time)




pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col =pt, group =pt)) +
    geom_line(aes(x = time, y = sld, col =pt, group =pt)) +
    theme_bw()


jm <- JointModel(
    longitudinal = LongitudinalSteinFojo(

        mu_bsld = prior_normal(log(60), 0.5),
        mu_ks = prior_normal(log(0.2), 0.5),
        mu_kg = prior_normal(log(0.2), 0.5),

        omega_bsld = prior_lognormal(log(0.1), 0.5),
        omega_ks = prior_lognormal(log(0.1), 0.5),
        omega_kg = prior_lognormal(log(0.1), 0.5),

        sigma = prior_lognormal(log(0.005), 0.5),
        centred = TRUE

    ),
    survival = SurvivalExponential(
        lambda = prior_lognormal(log(365 * (1 / 400)), 0.5)
    ),
    link = Link(
        link_ttg(prior_normal(-0.2, 0.5)),
        link_dsld(prior_normal(0.2, 0.5))
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
        threshold = 5
    )
)

## Sample from JointModel

set.seed(1231)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 700,
    iter_warmup = 1200,
    chains = 2,
    parallel_chains = 2
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
summary_post(mp@results, c("link_dsld", "sm_exp_lambda"))
summary_post(mp@results, c("link_ttg", "sm_exp_lambda"))


################################
#
# General Diagnostic stuff
#
#



mp@results$aummary(vars)


library(bayesplot)
mcmc_trace(mp@results$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp@results$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp@results$draws(c("link_ttg", "sm_exp_lambda")))

mcmc_pairs(mp@results$draws(), c("link_ttg", "sm_exp_lambda"))
