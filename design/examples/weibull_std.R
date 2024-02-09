

library(dplyr)
library(flexsurv)
library(survival)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)


dat <- flexsurv::bc |>
    as_tibble() |>
    mutate(arm = "A", study = "S", pt = sprintf("pt-%05d", 1:n()))



################################
#
# Cox Regression
#

coxph(
    Surv(recyrs, censrec) ~ group,
    data = dat
)


################################
#
# Flexsurv parametric Regression
#


flexsurvreg(
    Surv(recyrs, censrec) ~ group,
    data = dat,
    dist = "weibullPH"
)


################################
#
# Survreg parametric Regression
#


mod <- survreg(
    Surv(recyrs, censrec) ~ group,
    data = dat,
    dist = "weibull"
)

gamma <- 1 / mod$scale
lambda <- exp(-mod$coefficients[1] * gamma)
param_log_coefs <- -mod$coefficients[-1] * gamma
c(gamma, lambda, param_log_coefs)
## Need to use delta method to get standard errors



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
    times = dat$rectime,
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

fit$summary()


bayesplot::mcmc_pairs(fit$draws())

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





flexsurvreg(Surv(recyrs, censrec)~group, data=bc, dist="weibullph")
shape = 1.37965
scale = 0.03472
groupMedium = 0.84654
groupPoor = 1.67243

survHE.hmc.weiph<-fit.models(formula=Surv(recyrs, censrec)~group, data=bc, distr="weibullph", method="hmc")
shape = 1.3799683
scale = 0.0350171
0.8481786
1.6736389



normal(0, 20)

x <- rnorm(100000, 0, 2) |>
    exp() |>
    density() |>
    plot()
