library(jmpost)


my_long_mod <- exponential_long_model(functions = source_stan_part("exp_long_functions.stan"),
                                      data = source_stan_part("exp_long_data.stan"),
                                      parameters = source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)"),
                                      generated_quantities = "",
                                      inits = jmpost:::long_inits)
my_long_mod@inits$mean_mu_ks <- 1.5
my_long_mod@inits$mean_mu_kg <- .5
my_long_mod@inits$mean_mu_phi <- .3
my_long_mod@inits$omega_bsld <- .135
my_long_mod@inits$omega_ks <- .15
my_long_mod@inits$omega_kg <- .225
my_long_mod@inits$omega_phi <- .7
my_long_mod@inits$sd_mu_ks <- 2
my_long_mod@inits$sd_mu_kg <- 2
my_long_mod@inits$sd_mu_phi <- 2

my_os_mod <- jmpost:::temp_stan_os(functions = source_stan_part("os_functions.stan"),
                                   data = source_stan_part("os_data.stan"),
                                   parameters = source_stan_part("os_parameters.stan"),
                                   transformed_parameters = source_stan_part("os_transformed_parameters.stan"),
                                   prior = os_prior(),
                                   generated_quantities = source_stan_part("os_generated_quantities.stan"),
                                   inits = jmpost:::os_inits)
my_os_mod@inits$lambda <- .9
my_os_mod@inits$p <- 2.1

my_link_ttg <- get_link_TTG(my_long_mod)
my_link_dt <- get_link_DT(my_long_mod)

my_link_dt@inits$beta_dt <- 0.0035
my_link_ttg@inits$beta_ttg <- -1

# merge two link arguments
my_link <- merge_link(link1 = my_link_dt, link2 = my_link_ttg)

# Fill in the stanos with the link
my_final_os <- parametrize(osmod = my_os_mod, link = my_link)

# name it jm_merge eg because it is special. All all stuff here and convert lists to character already
my_jmpost <- jm_complete(my_long_mod, my_final_os)

library(cmdstanr)
model_v3 <- jm_compile(my_jmpost)

set.seed(1734)
sim_data_1 <- simulate_os_sld(N = 50,
                              lambda = 0.9,
                              p = 2.1,
                              beta = 0.0035,
                              gamma = 0,
                              psi_mu = c(b = 10, s = 1.5, g = 0.5, phi = 0.3),
                              psi_omega = c(b = 0.135, s = 0.15, g = 0.225, phi = 0.7),
)
sim_data_1$os$STUDYID <- factor(rep("study1", 50))
sim_data_1$os$trt <- factor(rep("TRT", 50))
sim_data_1$os$ARM <- sim_data_1$os$trt

set.seed(1834)
sim_data_2 <- simulate_os_sld(N = 50, pt_suffix = "suffix2")
sim_data_2$os$STUDYID <- factor(rep("study1", 50))
sim_data_2$os$trt <- factor(rep("PLA", 50))
sim_data_2$os$ARM <- sim_data_2$os$trt

sim_data <- list( os = rbind(sim_data_1$os, sim_data_2$os),
                  sld = rbind(sim_data_1$sld, sim_data_2$sld))

pre_data <- jm_data(sld = sim_data$sld,
                    os = sim_data$os,
                    vars = jmpost:::vars(longitudinal = "SLD",
                                         os_user_id = "pt",
                                         overall_survival_death = "event",
                                         long_user_id = "pt",
                                         time_survival = "time",
                                         os_study_id = "STUDYID",
                                         os_arm = "ARM",
                                         treatment = "trt"),
                    shared_treatment = "TRT",
                    censoring_threshold = 42)


patients_ids <- c(5)

my_mcmc_options <- jmpost:::mcmc_options(chains = 1,
                                         parallel_chains = 1,
                                         iter_warmup = 100,
                                         iter_sampling = 200,
                                         max_treedepth = 10,
                                         adapt_delta = .9,
                                         gauss_legendre = gauss_legendre())


my_mjpost <- jm_post(object = model_v3,
                     data = pre_data,
                     options = my_mcmc_options,
                     formula = ~ -1 + age + sex,
                     index_save_ind = patients_ids,
                     predictions  = seq(from = 0.001, to = 2, length = 100))


library(survival)
mod <- Surv(time = sim_data$os$time, event = sim_data$os$event)
f1 <- survfit(mod ~ trt, data = sim_data$os)
plot(f1,
     xlab = "Days",
     ylab = "Overall survival probability")
