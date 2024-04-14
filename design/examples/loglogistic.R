


library(dplyr)
library(flexsurv)
library(survival)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)

# library(jmpost)
devtools::load_all()


dat <- flexsurv::bc |>
    as_tibble() |>
    mutate(arm = "A", study = "S", pt = sprintf("pt-%05d", seq_len(n())))


#
#
# Have specified no covariates in this models so that they are comparable
# JMpost uses a PH model where as the others use AFT thus adding covariates
# would result in different models. Without covariates they are just fitting
# the base distribution which should be identical.
#
#




################################
#
# Flexsurv parametric Regression
#


mod_flex <- flexsurvreg(
    Surv(recyrs, censrec) ~ 1,
    data = dat,
    dist = "llogis"
)
mod_flex

logLik(mod_flex)
AIC(mod_flex)
BIC(mod_flex)


################################
#
# Bayesian Weibull Regression
#

mod <- cmdstan_model(
    stan_file = here("design/examples/loglogistic.stan"),
    exe_file = here("design/examples/models/loglogistic")
)

design_mat <- model.matrix(~ 1, data = dat)


stan_data <- list(
    n = nrow(dat),
    design = design_mat,
    p = ncol(design_mat),
    times = dat$recyrs,
    event_fl = dat$censrec
)
fit <- mod$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    refresh = 200,
    iter_warmup = 1000,
    iter_sampling = 1500
)
vars <- c(
    "beta_design",
    "alpha_0",
    "beta_0"
)
fit$summary(vars)


# Log Likelihood
log_lik <- fit$draws("log_lik", format = "draws_matrix") |>
    apply(1, sum) |>
    mean()
log_lik

# AIC
k <- 2
-2 * log_lik + k * (stan_data$p + 1) # +1 for the scale parameter

# BIC
((stan_data$p + 1) * log(stan_data$n)) + (-2 * log_lik)



################################
#
# JMpost
#



jm <- JointModel(
    survival = SurvivalLogLogistic()
)

jdat <- DataJoint(
    subject = DataSubject(
        data = dat,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = dat,
        formula = Surv(recyrs, censrec) ~ 1
    )
)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_warmup = 1000,
    iter_sampling = 1500,
    chains = 2,
    parallel_chains = 2
)

vars <- c(
    "sm_loglogis_a",
    "sm_loglogis_b"
)

stanobj <- as.CmdStanMCMC(mp)

x <- stanobj$summary(vars)

c(
    "scale" = x$mean[1],
    "shape" = x$mean[2]
)


# Log Likelihood
log_lik <- stanobj$draws("log_lik", format = "draws_matrix") |>
    apply(1, sum) |>
    mean()
log_lik

# AIC
k <- 2
-2 * log_lik + k * 2

# BIC
(2 * log(nrow(dat))) + (-2 * log_lik)
