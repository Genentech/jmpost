###########################################
#
#
# Non-linear model - Random Effects + Group Level
#
#
###########################################

library(brms)
library(dplyr)
library(posterior)
library(cmdstanr)
devtools::load_all() # library(jmpost)


n <- 300
mu_b <- c("A" = 60, "B" = 50)
mu_s <- c("A" = 0.5, "B" = 0.4)
mu_g <- c("A" = 0.2, "B" = 0.1)
omega_b <- 0.1
omega_s <- 0.1
omega_g <- 0.1
sigma <- 1.5
n_vis <- 4

sf_sld <- function(time, b, s, g) {
    s <- dplyr::if_else(time >= 0, s, 0)
    b * (exp(-s * time) + exp(g * time) - 1)
}

set.seed(22231)
baseline <- tibble(
    group = rep(c("A", "B"), each = n / 2),
    pt = sprintf("pt_%06i", seq_len(n)),
    b = rlnorm(n, log(mu_b[group]), omega_b),
    s = rlnorm(n, log(mu_s[group]), omega_s),
    g = rlnorm(n, log(mu_g[group]), omega_g),
)

dat_full <- tibble(
    pt = rep(sprintf("pt_%06i", seq_len(n)), each = n_vis),
    time = rep(seq(0, 3, length.out = n_vis), n)
) |>
    left_join(baseline, by = "pt") |>
    mutate(mu = sf_sld(time = time, b = b, s = s, g = g)) |>
    mutate(value = rnorm(n * n_vis, mu, sigma))

dat <- dat_full |>
    select(pt, value, time, group)


### Debug - Make sure patient profiles look sensible
# ggplot(data = filter(dat, pt %in% sample(dat$pt, 5)), aes(x = time, y = value, col = pt, group = pt)) +
#     geom_point() +
#     geom_line()


############## jmpost code


dat2 <- dat |> mutate(study = "S1")

dat_bl <- dat2 |>
    select(pt, group, study) |>
    group_by(pt) |>
    slice(1) |>
    ungroup()


jdat <- DataJoint(
    subject = DataSubject(
        data = dat_bl,
        subject = "pt",
        arm = "group",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = dat2,
        formula =  value ~ time,
        threshold = -99999
    )
)

# Define the required model
jm <- JointModel(
    longitudinal = LongitudinalSteinFojo(
        mu_bsld = prior_normal(log(mean(mu_b)), 0.6),
        mu_ks = prior_normal(log(mean(mu_s)), 0.6),
        mu_kg = prior_normal(log(mean(mu_g)), 0.6),
        omega_bsld = prior_lognormal(log(omega_b), 0.6),
        omega_ks = prior_lognormal(log(omega_s), 0.6),
        omega_kg = prior_lognormal(log(omega_g), 0.6),
        sigma = prior_lognormal(log(sigma), 0.6),
        centred = FALSE,
        scaled_variance = FALSE
    )
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


## Diagnostics to make sure the model works as expected
stanmod <- as.CmdStanMCMC(mp)

dat_plot <- baseline |>
    mutate(est_b = posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_bsld")) |> colMeans()) |>
    mutate(est_g = posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_kg")) |> colMeans()) |>
    mutate(est_s = posterior::as_draws_matrix(stanmod$draws("lm_sf_psi_ks")) |> colMeans())


ggplot(dat_plot, aes(x = b, y = est_b)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

ggplot(dat_plot, aes(x = g, y = est_g)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

ggplot(dat_plot, aes(x = s, y = est_s)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)


est_value <- posterior::as_draws_matrix(stanmod$draws("Ypred")) |> colMeans()

dat_plot <- dat_full |>
    mutate(est_value = est_value)

ggplot(dat_plot, aes(x = est_value, y = value)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)


############## BRMS code


### Default priors + internal data used by brms
###   - Was useful for knowing which options to set on the priors (e.g. class / nlpar / coef)
###   - This is just here for debugging / info purposes

#                  prior class      coef group resp dpar nlpar lb ub       source
#  student_t(3, 0, 15.1) sigma                                  0         default
#                 (flat)     b                               b            default
#                 (flat)     b    groupB                     b       (vectorized)
#                 (flat)     b Intercept                     b       (vectorized)
#  student_t(3, 0, 15.1)    sd                               b  0         default
#  student_t(3, 0, 15.1)    sd              pt               b  0    (vectorized)
#  student_t(3, 0, 15.1)    sd Intercept    pt               b  0    (vectorized)
#                 (flat)     b                               g            default
#                 (flat)     b    groupB                     g       (vectorized)
#                 (flat)     b Intercept                     g       (vectorized)
#  student_t(3, 0, 15.1)    sd                               g  0         default
#  student_t(3, 0, 15.1)    sd              pt               g  0    (vectorized)
#  student_t(3, 0, 15.1)    sd Intercept    pt               g  0    (vectorized)
#                 (flat)     b                               s            default
#                 (flat)     b    groupB                     s       (vectorized)
#                 (flat)     b Intercept                     s       (vectorized)
#  student_t(3, 0, 15.1)    sd                               s  0         default
#  student_t(3, 0, 15.1)    sd              pt               s  0    (vectorized)
#  student_t(3, 0, 15.1)    sd Intercept    pt               s  0    (vectorized)

set.seed(283)

#
#
# WARNING - Note that the brms model seems to be very sensitive to starting values
#           / priors. It is not uncommon for the model to produce non-sense results
#           if the chain gets stuck in a bad region of the parameter space.
#           Please check usual diagnostics to make sure the model is working as expected.
#           See an example of a bad fit here: https://github.com/Genentech/jmpost/pull/425
# 
bfit <- brm(
    bf(
        value ~ exp(b) * (exp(-exp(s) * time) + exp(exp(g) * time) - 1),
        b ~ 1 + group + (1 | pt),
        s ~ 1 + group + (1 | pt),
        g ~ 1 + group + (1 | pt),
        nl = TRUE
    ),
    data = dat,
    prior = c(
        prior("normal(log(60), 0.6)", nlpar = "b"),   # b intercept
        prior("normal(log(0.5), 0.6)", nlpar = "s"),  # s intercept
        prior("normal(log(0.2), 0.6)", nlpar = "g"),  # g intercept
        prior("normal(-0.1, 0.2)", nlpar = "b", class = "b", coef = "groupB"), # b group offset
        prior("normal(-0.3, 0.2)", nlpar = "s", class = "b", coef = "groupB"), # s group offset
        prior("normal(-0.7, 0.2)", nlpar = "g", class = "b", coef = "groupB"), # g group offset
        prior("lognormal(log(0.1), 0.6)", nlpar = "b", class = "sd"),  # b random effect sigma
        prior("lognormal(log(0.1), 0.6)", nlpar = "s", class = "sd"),  # s random effect sigma
        prior("lognormal(log(0.1), 0.6)", nlpar = "g", class = "sd"),  # g random effect sigma
        prior("lognormal(log(1.5), 0.6)", class = "sigma")   # overall sigma
    ),
    warmup = 750,
    iter = 1500,
    chains = 3,
    cores = 3,
    backend = "cmdstanr"
)

bfit


## Diagnostics to make sure the model works as expected
bfit_means <- posterior::as_draws_matrix(bfit) |>
    colMeans()

baseline_bfit <- baseline |>
    mutate(est_b = exp(bfit_means["b_b_Intercept"] + bfit_means["b_b_groupB"] * (group == "B") + bfit_means[grep("r_pt__b", names(bfit_means))])) |>
    mutate(est_s = exp(bfit_means["b_s_Intercept"] + bfit_means["b_s_groupB"] * (group == "B") + bfit_means[grep("r_pt__s", names(bfit_means))])) |>
    mutate(est_g = exp(bfit_means["b_g_Intercept"] + bfit_means["b_g_groupB"] * (group == "B") + bfit_means[grep("r_pt__g", names(bfit_means))]))



ggplot(baseline_bfit, aes(x = b, y = est_b)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

ggplot(baseline_bfit, aes(x = g, y = est_g)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

ggplot(baseline_bfit, aes(x = s, y = est_s)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)
