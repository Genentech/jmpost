
devtools::document()
devtools::load_all()

options("jmpost.cache.dir" = file.path("local", "models"))

library(bayesplot)

jlist <- simulate_joint_data(
    n_arm = c(1000, 1000),
    times = seq(1, 1000, by = 0.5),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.3,
        "C" = 0.5
    ),
    beta_cont = 0.2,
    lm_fun = sim_lm_random_slope(phi = 0),
    os_fun = sim_os_exponential(lambda = 1/100)
)

dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival = SurvivalExponential()
)

write_stan(jm, "local/debug.stan")

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
    )
)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 500,
    iter_warmup = 500,
    chains = 1,
    parallel_chains = 1
)


t_grid <- c(1, 50, 100, 400, 800)
sq <- SurvivalQuantities(
    mp,
    time_grid = t_grid,
    type = "surv"
)
brierScore(sq)



mod <- survival::survreg(
    survival::Surv(time, event) ~ cov_cont + cov_cat ,
    data = dat_os,
    dist = "exponential"
)
lambda <- exp(-predict(mod, type = "lp"))

p_times <- rep(t_grid, each = length(lambda))
p_lambda <- rep(lambda, times.out = length(t_grid))

pred_mat <- matrix(
    nrow = length(lambda),
    ncol = length(t_grid),
    pexp(p_times, rate = p_lambda)
)

brier_score(
    t = t_grid,
    times = dat_os$time,
    events = dat_os$event,
    pred_mat = pred_mat
)
