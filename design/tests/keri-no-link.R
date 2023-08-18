library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = FALSE)

options("jmpost.cache.dir" = file.path("local", "models"))

### Try to re-create analysis of Kerioui et. al 2020


keri_data_location <- here("local", "Keri-Simulated_Dataset.txt")

kdata <- read.table(keri_data_location, sep=",", header=TRUE)

k_data_os <- kdata %>%
    select(pt = ID, time = T, event = delta) %>%
    distinct(pt, time, event) %>%
    mutate(study = "Study-1", arm = rep(c("Group-1"), each = 100)) %>%
    mutate(pt = factor(pt))

k_data_lm <- kdata %>%
    select(pt = ID, time = Time, sld = SLD, observed = Censor_SLD) %>%
    mutate(pt = factor(pt), observed = if_else(observed == 0, TRUE, FALSE)) %>%
    left_join(k_data_os %>% distinct(pt, study, arm), by = "pt")


k_stan_data <- as_stan_data(k_data_os, k_data_lm, ~ 1)





jdat <- DataJoint(
    survival = DataSurvival(
        data = k_data_os,
        formula = Surv(time, event) ~ 1,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = k_data_lm,
        formula = sld ~ time,
        subject = "pt",
        threshold = 1
    )
)


jm <- JointModel(
    longitudinal = LongitudinalGSF(
        mu_bsld = prior_lognormal(log(60), 0.6),
        mu_ks = prior_lognormal(log(0.3), 0.6),
        mu_kg = prior_lognormal(log(0.2), 0.6),
        mu_phi = prior_beta(7, 10),

        omega_bsld = prior_lognormal(log(0.1), 0.6),
        omega_ks = prior_lognormal(log(0.1), 0.6),
        omega_kg = prior_lognormal(log(0.1), 0.6),
        omega_phi = prior_lognormal(log(0.1), 0.6),

        sigma = prior_lognormal(log(0.03), 0.6)
    )
)

mp_k <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 500,
    iter_warmup = 1000,
    chains = 4,
    parallel_chains = 4
)

compare_vars <- c(
    "lm_gsf_mu_bsld",
    "lm_gsf_mu_ks",
    "lm_gsf_mu_kg",
    "lm_gsf_mu_phi",
    "lm_gsf_omega_bsld",
    "lm_gsf_omega_ks",
    "lm_gsf_omega_kg",
    "lm_gsf_omega_phi",
    "lm_gsf_sigma"
)


mp_k@results$summary(compare_vars)
