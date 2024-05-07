
library(jmpost)
library(dplyr)
library(ggplot2)
library(tidyr)


############################
#
# Simulated Data Setup
#
#

set.seed(7143)

jlist <- SimJointData(
    design = list(
        SimGroup(100, "Arm-A", "Study-X"),
        SimGroup(100, "Arm-B", "Study-X")
    ),
    survival = SimSurvivalExponential(
        lambda = 1 / (400 / 365),
        time_max = 3,
        time_step = 1 / 365,
        lambda_censor = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3
    ),
    longitudinal = SimLongitudinalGSF(
        # Large number of time points so that we can subset later to have random timepoints
        # per subject
        times = c(
            -50, -10, -5, -4, -3, -2, -1, 0, 1, 2, 5, 6, 10,
            25, 30, 40, 50, 75, 80, 100, 110, 125, 150, 175, 200, 225, 250, 275,
            300, 350, 400, 450, 500, 550, 600, 700, 800, 900,
            1000, 1100
        ) / 365,
        sigma = 0.01,
        mu_s = c(0.6, 0.4),
        mu_g = c(0.25, 0.35),
        mu_b = 60,
        a_phi = c(20, 15),
        b_phi = c(15, 20),
        omega_b = 0.2,
        omega_s = 0.2,
        omega_g = 0.2,
        link_dsld = 0,
        link_ttg = 0,
        link_identity = 0
    ),
    .silent = TRUE
)


# Ensuring that we have 1 baseline observation per subject and then a
# random number of post baseline observations
dat_lm_bl <- jlist@longitudinal |>
    filter(time <= 10 / 365) |>
    group_by(pt) |>
    sample_n(1) |>
    ungroup()

dat_lm <- jlist@longitudinal |>
    filter(time > 10 / 365, observed) |>
    sample_frac(0.25) |>
    bind_rows(dat_lm_bl) |>
    arrange(pt, time)

dat_os <- jlist@survival |>
    filter(pt %in% dat_lm$pt)



############################
#
# Fitting Joint Model
#
#

jdat <- DataJoint(
    subject = DataSubject(
        data = jlist@survival,
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
        formula = sld ~ time
    )
)

# Define the required model
jm <- JointModel(
    longitudinal = LongitudinalGSF(
        mu_bsld = prior_normal(log(60), 1),
        mu_ks = prior_normal(log(0.6), 1),
        mu_kg = prior_normal(log(0.3), 1),
        omega_bsld = prior_lognormal(log(0.2), 1),
        omega_ks = prior_lognormal(log(0.2), 1),
        omega_kg = prior_lognormal(log(0.2), 1),
        a_phi = prior_lognormal(log(18), 1),
        b_phi = prior_lognormal(log(18), 1),
        sigma = prior_lognormal(log(0.01), 1),
        centred = FALSE
    ),
    survival = SurvivalExponential(
        lambda = prior_lognormal(log(1 / (400 / 365)), 1)
    ),
    link = Link()
)


mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_warmup = 500,
    iter_sampling = 1000,
    chains = 3,
    refresh = 100,
    parallel_chains = 3
)

# get the raw stan object for subsequent processing
stanobj <- as.CmdStanMCMC(mp)



############################
#
# Plot 1 - Predicted vs Observed
#
#

longquant_obvs <- LongitudinalQuantities(
    mp,
    grid = GridObserved()
)

dat_quant_obvs <- summary(longquant_obvs) |>
    as_tibble() |>
    left_join(dat_lm, by = c("group" = "pt", "time"))


ggplot(
    dat_quant_obvs,
    aes(x = median, y = sld, xmin = lower, xmax = upper, group = interaction(group, time))
) +
    geom_errorbarh(col = "#c6c625") +
    geom_point(alpha = 0.4) +
    theme_bw() +
    geom_abline(intercept = 0, slope = 1, col = "black", lty = "dashed") +
    xlab("Predicted SLD (mm)") +
    ylab("Observed SLD (mm)")


############################
#
# Plot 2 - Predicted vs Observed over time
#
#

sampled_subjects <- sample(dat_lm$pt, 4)

longquant_fixed <- LongitudinalQuantities(
    mp,
    grid = GridEven(
        subjects = sampled_subjects,
        length.out = 40
    )
)

autoplot(longquant_fixed) +
    ylab("Observed SLD (mm)") +
    xlab("Time (years)")


## Alternative if you want more customisation

dat_quant_fixed <- summary(longquant_fixed) |>
    as_tibble()

dat_os_plot <- dat_os |>
    mutate(group = pt) |>
    filter(pt %in% sampled_subjects) |>
    mutate(event_chr = ifelse(event == 1, "Event", "Censored")) |>
    mutate(event_chr = factor(
        event_chr,
        labels = c("Event", "Censored"),
        levels = c("Event", "Censored")
    ))

ggplot() +
    geom_ribbon(
        data = dat_quant_fixed,
        aes(x = time, ymin = lower, ymax = upper, group = group),
        alpha = 0.3,
        fill = "#fffb78"
    ) +
    geom_line(
        data = dat_quant_fixed,
        aes(x = time, y = median, group = group),
        col = "#81d681"
    ) +
    geom_point(
        data = dat_lm |> mutate(group = pt) |> filter(pt %in% sampled_subjects),
        aes(x = time, y = sld, group = pt),
        col = "#9c3abf"
    ) +
    geom_vline(
        data = dat_os_plot,
        mapping = aes(xintercept = time, lty = event_chr),
        col = "#a40000"
    ) +
    theme_bw() +
    facet_wrap(~group) +
    ylab("SLD (mm)") +
    xlab("Time (years)") +
    scale_linetype_discrete(name = "")





############################
#
# Plot 3 - Population Level
#
#

select_times <- seq(-50, 500, length.out = 100) / 365
select_times_reduced <- select_times[seq(0, 100, length.out = 8)]

longquant_pop <- LongitudinalQuantities(
    mp,
    grid = GridPopulation(
        times = select_times
    )
)


autoplot(longquant_pop) +
    ylab("SLD (mm)") +
    xlab("Time (years)")


## Alternative if want more manual control / styling

pdat_sample <- as.data.frame(longquant_pop) |>
    as_tibble() |>
    group_by(group, time) |>
    mutate(id = seq_len(n()))

pdat_summary <- summary(longquant_pop) |>
    as_tibble() |>
    filter(time %in% select_times_reduced)

ggplot() +
    geom_line(
        data = pdat,
        aes(x = time, y = values, group = id),
        alpha = 0.05,
        col = "#60e160"
    ) +
    geom_line(
        data = pdat_summary,
        aes(x = time, y = median),
        col = "#7e1abd"
    ) +
    geom_point(
        data = pdat_summary,
        aes(x = time, y = median),
        col = "#7e1abd",
        alpha = 0.9,
        size = 3
    ) +
    geom_errorbar(
        data = pdat_summary,
        aes(x = time, ymin = lower, ymax = upper),
        width = 0,
        col = "#7e1abd",
        alpha = 0.7
    ) +
    facet_wrap(~group) +
    theme_bw()



############################
#
# Plot 4 - Median of individuals
#
#

longquant_fixed <- LongitudinalQuantities(
    mp,
    grid = GridFixed(
        times = seq(0, 3, by = 0.1)
    )
)

longquant_fixed_sum <- summary(longquant_fixed) |>
    as_tibble() |>
    group_by(time) |> # Group by additional variables as required e.g. EGFR status
    summarise(
        median_lq = quantile(median, 0.025),
        median_med = median(median),
        median_med = quantile(median, 0.975),
        lower_lq = quantile(lower, 0.025),
        lower_med = median(lower),
        lower_uq = quantile(lower, 0.975),
        upper_lq = quantile(upper, 0.025),
        upper_med = median(upper),
        upper_uq = quantile(upper, 0.975),
        .groups = "drop"
    )


ggplot() +
    geom_ribbon(
        data = longquant_fixed_sum,
        aes(x = time, ymin = lower_lq, ymax = lower_uq),
        alpha = 0.6,
        fill = "#8af6a9"
    ) +
    geom_ribbon(
        data = longquant_fixed_sum,
        aes(x = time, ymin = upper_lq, ymax = upper_uq),
        alpha = 0.6,
        fill = "#8af6a9"
    ) +
    geom_line(
        data = dat_lm,
        aes(x = time, y = sld, group = pt),
        alpha = 0.7,
        size = 0.2
    ) +
    geom_line(
        data = longquant_fixed_sum,
        aes(x = time, y = median_med),
        size = 0.7,
        col = "#23a123"
    ) +
    theme_bw() +
    xlab("Time (Years)") +
    ylab("SLD (mm)")
