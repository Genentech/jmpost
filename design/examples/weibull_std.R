

library(dplyr)
library(flexsurv)
library(survival)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)
library(brms)


dat <- flexsurv::bc |>
    as_tibble() |>
    mutate(arm = "A", study = "S", pt = sprintf("pt-%05d", 1:n()))



################################
#
# Cox Regression
#

mod_cox <- coxph(
    Surv(recyrs, censrec) ~ group,
    data = dat
)


################################
#
# Flexsurv parametric Regression
#


mod_flex <- flexsurvreg(
    Surv(recyrs, censrec) ~ group,
    data = dat,
    dist = "weibullPH"
)
AIC(mod_flex)
BIC(mod_flex)
logLik(mod_flex)

################################
#
# Survreg parametric Regression
#


mod_surv <- survreg(
    Surv(recyrs, censrec) ~ group,
    data = dat,
    dist = "weibull"
)

gamma <- 1 / mod_surv$scale
lambda <- exp(-mod_surv$coefficients[1] * gamma)
param_log_coefs <- -mod_surv$coefficients[-1] * gamma

c(gamma, lambda, param_log_coefs)
## Need to use delta method to get standard errors


AIC(mod_surv)
BIC(mod_surv)
logLik(mod_surv)



################################
#
# brms Weibull Regression
#

mod_brms <- brm(
    recyrs | cens(censored) ~ group,
    dat |> mutate(censored = if_else(censrec == 1, "none", "right")),
    family = weibull(),
    cores = 4,
    warmup = 1000,
    iter = 2000,
    seed = 7819
)

brms_pars <- as_draws_matrix(mod_brms)[, c("shape", "b_Intercept", "b_groupMedium", "b_groupPoor")] |>
    apply(2, mean)

brms_gamma <- brms_pars[["shape"]]
brms_lambda <- exp(-brms_pars[["b_Intercept"]] * brms_gamma)
brms_param_log_coefs <- -brms_pars[c("b_groupMedium", "b_groupPoor")] * brms_gamma

c(
    "gamma"= brms_gamma,
    "lambda" = brms_lambda,
    brms_param_log_coefs
)

# Leave one out CV
loo(mod_brms)



################################
#
# Bayesian Weibull Regression
#

mod <- cmdstan_model(
    stan_file = here("design/examples/weibull.stan"),
    exe_file = here("design/examples/models/weibull")
)

design_mat <- model.matrix(~ group, data = dat)


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
vars_stan <- c(
    "gamma_0",
    "lambda_0",
    "beta"
)
fit$summary(vars_stan)


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


# Leave one out CV
fit$loo()


################################
#
# JMpost
#

devtools::load_all()
# library(jmpost)

jm <- JointModel(
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(log(1/200), 1.3),
        gamma = prior_lognormal(log(1), 1.3)
    )
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
        formula = Surv(recyrs, censrec) ~ group
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
    "sm_weibull_ph_lambda",
    "sm_weibull_ph_gamma",
    "beta_os_cov"
)

mp@results$summary(vars)

# Log Likelihood
log_lik <- mp@results$draws("log_lik", format = "draws_matrix") |>
    apply(1, sum) |>
    mean()
log_lik

# AIC
k <- 2
-2 * log_lik + k * 4

# BIC
(4 * log(nrow(dat))) + (-2 * log_lik)

# Leave one out CV
mp@results$loo()
