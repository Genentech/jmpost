
#library(jmpost)
devtools::load_all()
library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(7143)

jlist <- SimJointData(
    design = list(
        SimGroup(100, "Arm-A", "Study-X"),
        SimGroup(100, "Arm-B", "Study-X")
    ),
    survival = SimSurvivalWeibullPH(
        lambda = 1 / 1.5,
        gamma = 0.95,
        time_max = 3,
        time_step = 1 / 365,
        lambda_censor = 1 / 1.5,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3
    ),
    longitudinal = SimLongitudinalGSF(
        times = c(
            1, 25, 50, 75, 100, 150, 200, 250, 300, 350, 450, 600, 750, 900, 1100
        ) / 365,
        sigma = 0.01,
        mu_s = log(c(0.6, 0.4)),
        mu_g = log(c(0.25, 0.35)),
        mu_b = log(60),
        mu_phi = qlogis(c(0.4, 0.6)),
        omega_b = 0.2,
        omega_s = 0.2,
        omega_g = 0.2,
        omega_phi = 0.2,
        link_dsld = 0.03,
        link_ttg = 0.11,
        link_identity = 0
    )
)

mean(jlist@survival$event)
median(jlist@survival$time)

dat_plot <- jlist@longitudinal |>
    filter(subject %in% sample(jlist@survival$subject, 6))

ggplot(dat_plot, aes(x = time, y = sld, group = subject, col = subject)) +
    geom_point() +
    geom_line() +
    theme_bw()



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
        formula = sld ~ time
    )
)

# Define the required model
jm <- JointModel(
    longitudinal = LongitudinalGSF(
        mu_bsld = prior_normal(log(60), 0.5),
        mu_ks = prior_normal(log(0.6), 0.5),
        mu_kg = prior_normal(log(0.3), 0.5),
        mu_phi = prior_normal(0, 0.5),
        omega_bsld = prior_lognormal(log(0.2), 0.5),
        omega_ks = prior_lognormal(log(0.2), 0.5),
        omega_kg = prior_lognormal(log(0.2), 0.5),
        omega_phi = prior_lognormal(log(0.2), 0.5),
        sigma = prior_lognormal(log(0.01), 0.5),
        centred = TRUE
    ),
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(1 / 1.5, 0.5),
        gamma = prior_lognormal(0.95, 0.5)
    ),
    link = Link(
        linkTTG(prior = prior_normal(-0.2, 0.2)),
        linkDSLD(prior = prior_normal(0.03, 0.05))
    )
)


mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_warmup = 700,
    iter_sampling = 1000,
    chains = 3,
    refresh = 100,
    parallel_chains = 3
)







# Define the required model
jm_surv <- JointModel(
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(1 / 1.5, 0.5),
        gamma = prior_lognormal(0.95, 0.5)
    )
)

mp_surv <- sampleStanModel(
    jm_surv,
    data = jdat,
    iter_warmup = 700,
    iter_sampling = 1000,
    chains = 3,
    refresh = 100,
    parallel_chains = 3
)



print(as.CmdStanMCMC(mp), c("link_dsld", "link_ttg"))
print(as.CmdStanMCMC(mp), c("sm_weibull_ph_gamma", "sm_weibull_ph_lambda"))
print(as.CmdStanMCMC(mp_surv), c("sm_weibull_ph_gamma", "sm_weibull_ph_lambda"))



time_span <- seq(0, 2.5, by = 0.1)

sonly_surv <- SurvivalQuantities(
    mp_surv,
    GridGrouped(
        split(jlist@survival$subject, jlist@survival$arm),
        times = time_span
    ),
    type = "surv"
)

joint_surv <- SurvivalQuantities(
    mp,
    GridGrouped(
        split(jlist@survival$subject, jlist@survival$arm),
        times = time_span
    ),
    type = "surv"
)

sonly_surv_dat <- summary(sonly_surv)
joint_surv_dat <- summary(joint_surv)









sonly_surv_dat$type <- "surv only"
joint_surv_dat$type <- "joint"

surv_dat <- bind_rows(sonly_surv_dat, joint_surv_dat)
ggplot(surv_dat, aes(x = time, y = median, ymin = lower, ymax = upper, group = type, col = type, fill = type)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    facet_wrap(~group) +
    theme_bw()


surv_dat |>
    filter(median >= 0.5) |>
    group_by(group, type) |>
    arrange((median)) |>
    slice(1) |>
    mutate(ci_width = upper - lower) |>
    arrange(type, group)



time_span <- seq(0.1, 2.5, by = 0.1)

sonly_haz <- SurvivalQuantities(
    mp_surv,
    GridGrouped(
        split(jlist@survival$subject, jlist@survival$arm),
        times = time_span
    ),
    type = "haz"
)

joint_haz <- SurvivalQuantities(
    mp,
    GridGrouped(
        split(jlist@survival$subject, jlist@survival$arm),
        times = time_span
    ),
    type = "haz"
)


sonly_haz_dat <- as.data.frame(sonly_haz) |>
    as_tibble() |>
    group_by(group) |>
    mutate(index = seq_len(n())) |>
    tidyr::spread(group, values) |>
    mutate(hr = `Arm-A` / `Arm-B`) |>
    group_by(time) |>
    summarise(
        lower = quantile(hr, 0.025),
        median = quantile(hr, 0.5),
        upper = quantile(hr, 0.975)
    ) |>
    mutate(type = "surv only")



joint_haz_dat <- as.data.frame(joint_haz) |>
    as_tibble() |>
    group_by(group) |>
    mutate(index = seq_len(n())) |>
    tidyr::spread(group, values) |>
    mutate(hr = `Arm-A` / `Arm-B`) |>
    group_by(time) |>
    summarise(
        lower = quantile(hr, 0.025),
        median = quantile(hr, 0.5),
        upper = quantile(hr, 0.975)
    ) |>
    mutate(type = "joint")

pdat <- bind_rows(sonly_haz_dat, joint_haz_dat)

ggplot(pdat, aes(x = time, y = median, ymin = lower, ymax = upper)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    facet_wrap(~type) +
    theme_bw()






print(as.CmdStanMCMC(mp_surv), c("sm_weibull_ph_gamma", "sm_weibull_ph_lambda"))


flexsurv::hweibullPH(time_span, 1.1, 0.79)







jlist <- SimJointData(
    design = list(
        SimGroup(500, "Arm-A", "Study-X"),
        SimGroup(500, "Arm-B", "Study-X")
    ),
    survival = SimSurvivalWeibullPH(
        lambda = 1 / 1.5,
        gamma = 0.95,
        time_max = 3,
        time_step = 1 / 365,
        lambda_censor = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = 0.3
        ),
        beta_cont = 0.3
    ),
    longitudinal = SimLongitudinalGSF(
        times = c(
            1, 25, 50, 75, 100, 150, 200, 250, 300, 350, 450, 600, 750, 900, 1100
        ) / 365,
        sigma = 0.01,
        mu_s = log(c(0.6, 0.4)),
        mu_g = log(c(0.25, 0.35)),
        mu_b = log(60),
        mu_phi = qlogis(c(0.4, 0.6)),
        omega_b = 0.2,
        omega_s = 0.2,
        omega_g = 0.2,
        omega_phi = 0.2,
        link_dsld = 0,
        link_ttg = 0,
        link_identity = 0
    )
)

dat_surv <- jlist@survival |>
    filter((arm == "Arm-A" & cov_cat == "A") | (arm == "Arm-B" & cov_cat == "B")) |>
    group_by(arm) |>
    sample_n(150) |>
    ungroup()


jdat <- DataJoint(
    subject = DataSubject(
        data = dat_surv,
        subject = "subject",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = dat_surv,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    )
)

# Define the required model
jm_surv <- JointModel(
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(1 / 1.5, 0.5),
        gamm = prior_lognormal(0.95, 0.5)
    )
)

mp_surv <- sampleStanModel(
    jm_surv,
    data = jdat,
    iter_warmup = 700,
    iter_sampling = 1000,
    chains = 3,
    refresh = 100,
    parallel_chains = 3
)

as.CmdStanMCMC(mp_surv)
