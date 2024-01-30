

library(dplyr)
library(flexsurv)
library(survival)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)


n <- 1000

log_hr_trt <- c(
    "placebo" = 0,
    "active" = -0.3
)
log_hr_sex <- c(
    "M" = 0,
    "F" = 0.2
)
log_hr_age <- 0.1

lambda_bl <- 1 / 200
gamma_bl <- 0.95

dat <- tibble(
    pt = sprintf("pt-%05d", 1:n),
    trt = sample(names(log_hr_trt), size = n, replace = TRUE, prob = c(0.5, 0.5)),
    age = rnorm(n),
    sex = sample(names(log_hr_sex), size = n, replace = TRUE, prob = c(0.4, 0.6)),
    HR = exp(
        log(lambda_bl) +
            log_hr_age * age +
            log_hr_sex[sex] +
            log_hr_trt[trt]
    ),
    time = rweibullPH(n, scale = HR, shape = gamma_bl),
    centime = rexp(n, 1 / 400)
) |>
    mutate(event = ifelse(time <= centime, 1, 0)) |>
    mutate(time = ifelse(time <= centime, time, centime)) |>
    mutate(sex = factor(sex, levels = names(log_hr_sex))) |>
    mutate(trt = factor(trt, levels = names(log_hr_trt))) |>
    mutate(study = "Study-1")



################################
#
# Cox Regression
#

coxph(
    Surv(time, event) ~ age + sex + trt,
    data =dat
)


################################
#
# Bayesian Weibull Regression
#

mod <- cmdstan_model(
    stan_file = here("design/examples/weibull.stan"),
    exe_file = here("design/examples/models/weibull")
)

design_mat <- model.matrix(~ age + sex + trt, data = dat)


stan_data <- list(
    n = nrow(dat),
    x = rnorm(50, 5, 2),
    design = design_mat,
    p = ncol(design_mat),
    times = dat$time,
    event_fl = dat$event
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


################################
#
# JMpost
#

devtools::load_all()
# library(jmpost)

jm <- JointModel(
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(log(1 / 200), 0.5),
        gamma = prior_lognormal(log(0.95), 0.5)
    )
)

jdat <- DataJoint(
    subject = DataSubject(
        data = dat,
        subject = "pt",
        arm = "trt",
        study = "study"
    ),
    survival = DataSurvival(
        data = dat,
        formula = Surv(time, event) ~ age + sex + trt
    )
)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_warmup = 1000,
    iter_sampling = 1500,
    chains = 2
)

vars <- c(
    "sm_weibull_ph_lambda",   # 0.005
    "sm_weibull_ph_gamma",    # 0.95
    "beta_os_cov"             # 0.1,  0.3,  -0.2
)

mp@results$summary(vars)
