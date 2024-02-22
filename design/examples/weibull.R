

library(dplyr)
library(flexsurv)
library(survival)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)
library(brms)
library(tidyr)
# library(jmpost)
devtools::load_all()


dat <- flexsurv::bc |>
    as_tibble() |>
    mutate(arm = "A", study = "S", pt = sprintf("pt-%05d", 1:n()))


#
#
# Commented out code below generates a simulated dataset with known
# Parameter values.
#
#


# n <- 1000

# log_hr_trt <- c(
#     "placebo" = 0,
#     "active" = -0.3
# )
# log_hr_sex <- c(
#     "M" = 0,
#     "F" = 0.2
# )
# log_hr_age <- 0.1

# lambda_bl <- 1 / 200
# gamma_bl <- 0.95

# dat <- tibble(
#     pt = sprintf("pt-%05d", 1:n),
#     trt = sample(names(log_hr_trt), size = n, replace = TRUE, prob = c(0.5, 0.5)),
#     age = rnorm(n),
#     sex = sample(names(log_hr_sex), size = n, replace = TRUE, prob = c(0.4, 0.6)),
#     HR = exp(
#         log(lambda_bl) +
#             log_hr_age * age +
#             log_hr_sex[sex] +
#             log_hr_trt[trt]
#     ),
#     time = rweibullPH(n, scale = HR, shape = gamma_bl),
#     centime = rexp(n, 1 / 400)
# ) |>
#     mutate(event = ifelse(time <= centime, 1, 0)) |>
#     mutate(time = ifelse(time <= centime, time, centime)) |>
#     mutate(sex = factor(sex, levels = names(log_hr_sex))) |>
#     mutate(trt = factor(trt, levels = names(log_hr_trt))) |>
#     mutate(study = "Study-1")




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


#### Extract Desired Quantities

# Lambda here represents the fitted lambda for each individual subject separately
lambda <- fit$draws("lambda", format = "draws_df") |>
    as_tibble() |>
    gather(KEY, lambda, -.draw, -.iteration, -.chain) |>
    mutate(pt_index = str_extract(KEY, "\\d+"))

# gamma_0 is common to all subjects
gamma <- fit$draws("gamma_0", format = "draws_df") |>
    as_tibble()

# Combine gamma_0 and lambda so we have 1 row per subject per sample
# Also re-attach the PT variable to have a clear subject label
pt_map <- levels(factor(dat$pt))
samples <- left_join(lambda, gamma, by = c(".chain", ".draw", ".iteration")) |>
    mutate(pt = pt_map[as.numeric(pt_index)])


# Reduce the dataset down to just 2 subjects that we will create predictions for
samples_reduced <- samples |>
    filter(pt %in% c("pt-00681", "pt-00002"))

# Time points to evaluate their predictions at
target_times <- seq(min(dat$recyrs), max(dat$recyrs), length.out = 20)

# Duplicate dataset once per desired timepoint
samples_all_times <- bind_rows(
    lapply(target_times, \(t) samples_reduced |> mutate(time = t))
)

# Calculate the survival distribution for each subject at each desired timepoint
# To get different quantities change the `pweibullPH` to the desired distribution
# function e.g. hweibullPH / HweibullPH
survival_times <- samples_all_times |>
    mutate(surv = flexsurv::pweibullPH(time, gamma_0, lambda, lower.tail = FALSE)) |>
    group_by(pt, time) |>
    summarise(
        lci = quantile(surv, 0.025),
        med = quantile(surv, 0.5),
        uci = quantile(surv, 0.975),
        .groups = "drop"
    )

ggplot(
    data = survival_times,
    aes(x = time, y = med, ymin = lci, ymax = uci, group = pt, col = pt, fill = pt)
) +
    geom_line() +
    geom_ribbon(alpha = 0.4, col = NA) +
    theme_bw()





################################
#
# JMpost
#


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


#### Extract Desired Quantities

prediction_times <- seq(min(dat$recyrs), max(dat$recyrs), length.out = 20)
selected_patients <- c("pt-00681", "pt-00002")

# Survival plots
sq_surv <- SurvivalQuantities(
    mp,
    time_grid = prediction_times,
    groups = selected_patients,
    type = "surv"
)
autoplot(sq_surv, add_km = FALSE, add_wrap = FALSE)
summary(sq_surv)
# as.data.frame(sq_surv)  # Raw sample data

# Hazard
sq_haz <- SurvivalQuantities(
    mp,
    time_grid = prediction_times,
    groups = selected_patients,
    type = "haz"
)
autoplot(sq_haz, add_km = FALSE, add_wrap = FALSE)
