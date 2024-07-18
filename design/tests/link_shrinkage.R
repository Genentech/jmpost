library(dplyr)
library(ggplot2)
library(survival)
devtools::load_all()


model_pars <- list(
    sigma = 0.01,
    mu_s = log(c(0.2, 0.55)),
    mu_g = log(c(0.25, 0.15)),
    mu_b = log(60),
    omega_b = 0.1,
    omega_s = 0.1,
    omega_g = 0.1,
    link_ttg = 0,
    link_dsld = 0,
    link_shrinkage = 0.5,
    lambda = 1.2
)



set.seed(9532)
## Generate Test data with known parameters
jlist <- SimJointData(
    design = list(
        SimGroup(120, "Arm-A", "Study-X"),
        SimGroup(120, "Arm-B", "Study-X")
    ),
    longitudinal = SimLongitudinalSteinFojo(
        times = c(-100, -50, -10, 1, 100, 150, 200, 300, 400, 500, 600, 700, 800, 900) * (1 / 365),
        sigma = model_pars$sigma,
        mu_s = model_pars$mu_s,
        mu_g = model_pars$mu_g,
        mu_b = model_pars$mu_b,
        omega_b = model_pars$omega_b,
        omega_s = model_pars$omega_s,
        omega_g = model_pars$omega_g,
        link_ttg = model_pars$link_ttg,
        link_dsld = model_pars$link_dsld,
        link_shrinkage = model_pars$link_shrinkage
    ),
    survival = SimSurvivalExponential(
        time_max = 6,
        time_step = 1 / 365,
        lambda = model_pars$lambda,
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3
    )
)
dat_os <- jlist@survival
dat_lm <- jlist@longitudinal

median(dat_os$time)
mean(dat_os$event)
survfit(Surv(time, event) ~ arm, data = dat_os) |>
    plot()


pdat <- dat_lm |> filter(subject %in% sample(jlist@survival$subject, 7))
ggplot(pdat, aes(x = time, y = sld, group = subject, col = subject)) +
    geom_point() +
    geom_line() +
    theme_bw()





jm <- JointModel(
    longitudinal = LongitudinalSteinFojo(

        mu_bsld = prior_normal(log(60), 0.5),
        mu_ks = prior_normal(log(0.4), 0.5),
        mu_kg = prior_normal(log(0.2), 0.5),

        omega_bsld = prior_lognormal(log(0.1), 0.5),
        omega_ks = prior_lognormal(log(0.1), 0.5),
        omega_kg = prior_lognormal(log(0.1), 0.5),

        sigma = prior_lognormal(log(0.01), 0.5),
        centred = TRUE

    ),
    survival = SurvivalExponential(
        lambda = prior_lognormal(1, 0.5)
    ),
    link = Link(
        linkShrinkage(prior = prior_normal(0.5, 0.5))
    )
)

jdat <- DataJoint(
    subject = DataSubject(
        data = jlist@survival,
        subject = "subject",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = jlist@survival,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    ),
    longitudinal = DataLongitudinal(
        data = jlist@longitudinal,
        formula = sld ~ time,
        threshold = 5
    )
)

## Sample from JointModel

set.seed(2213)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 700,
    iter_warmup = 1000,
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

dat <- summary_post(
    as.CmdStanMCMC(mp),
    c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg"),
    FALSE
)
true_values <- c(model_pars$mu_b, model_pars$mu_s, model_pars$mu_g)
expect_true(all(dat$q01 <= true_values))
expect_true(all(dat$q99 >= true_values))
expect_true(all(dat$ess_bulk > 100))

dat <- summary_post(
    as.CmdStanMCMC(mp),
    c("link_shrinkage", "sm_exp_lambda")
)

true_values <- c(model_pars$link_shrinkage, model_pars$lambda)
expect_true(all(dat$q01 <= true_values))
expect_true(all(dat$q99 >= true_values))
expect_true(all(dat$ess_bulk > 100))




sq <- SurvivalQuantities(
    mp,
    GridGrouped(
        split(dat_os$subject, dat_os$arm),
        times = seq(0, 4, length.out = 30)
    )
)

autoplot(sq, add_wrap = FALSE, add_km = TRUE)
