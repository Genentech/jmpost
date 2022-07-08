library(jmpost)


my_long_mod <- exponential_long_model(functions = source_stan_part("exp_long_functions.stan"),
                                      data = source_stan_part("exp_long_data.stan"),
                                      parameters = source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)",
                                                         mean_mu_ks = "lognormal(3,0.5)",
                                                         mu_bsld = "lognormal( 8, 2)"),
                                      generated_quantities = "",
                                      inits = jmpost:::long_inits)

my_os_mod <- jmpost:::temp_stan_os(functions = source_stan_part("os_functions.stan"),
                                   data = source_stan_part("os_data.stan"),
                                   parameters = source_stan_part("os_parameters.stan"),
                                   transformed_parameters = source_stan_part("os_transformed_parameters.stan"),
                                   prior = os_prior(),
                                   generated_quantities = source_stan_part("os_generated_quantities.stan"),
                                   inits = jmpost:::os_inits)


my_link_ttg <- get_link_TTG(my_long_mod)
my_link_dt <- get_link_DT(my_long_mod)

# merge two link arguments
my_link <- merge_link(link1 = my_link_dt, link2 = my_link_ttg)

# Fill in the stanos with the link
my_final_os <- parametrize(osmod = my_os_mod, link = my_link)

# name it jm_merge eg because it is special. All all stuff here and convert lists to character already
my_jmpost <- jm_complete(my_long_mod, my_final_os)

library(cmdstanr)
model_v3 <- jm_compile(my_jmpost)

model_v3@inits <- list( mean_mu_ks = 5,
                        mean_mu_kg = 0.7,
                        mean_mu_phi = 0.1,
                        sd_mu_ks = 0.1,
                        sd_mu_kg = 0.1,
                        sd_mu_phi = 0.1,
                        mu_bsld = function(x) rep(55, x@data$n_studies),
                        mu_ks = function(x) rep(2, x@data$n_arms),
                        mu_kg = function(x) rep(.7, x@data$n_arms),
                        mu_phi = function(x) rep(.1, x@data$n_arms),
                        omega_bsld = 0.1,
                        omega_ks = 0.1,
                        omega_kg = 0.1,
                        omega_phi = 0.1,
                        sigma = 1,
                        eta_tilde_bsld = function(x) rep(0, x@data$Nind),
                        eta_tilde_ks = function(x) rep(0, x@data$Nind),
                        eta_tilde_kg = function(x) rep(0, x@data$Nind),
                        eta_tilde_phi = function(x) rep(0, x@data$Nind),
                        lambda = .5,
                        p = 1,
                        beta_os_cov = function(x) rep(0, ncol(x@data$os_cov_design)),
                        beta_ttg = 0,
                        beta_dt = 0 )


sim_data_1 <- simulate_os_sld(N = 50,
                              lambda = 0.534,
                              p = 2.1,
                              beta = 0.004,
                              gamma = 0,
                              psi_mu = c(b = 66, s = 2.4, g = 2, phi = 0.2),
                              psi_omega = c(b = 0.135, s = 0.15, g = 0.7, phi = 0.7)
)
sim_data_1$os$STUDYID <- factor(rep("study1", 50))
sim_data_1$os$trt <- factor(rep("PLA", 50))
sim_data_1$os$ARM <- sim_data_1$os$trt


sim_data_2 <- simulate_os_sld(N = 50,
                              lambda = 0.534,
                              p = 2.1,
                              beta = 0.004,
                              gamma = 0,
                              psi_mu = c(b = 52, s = 2.2, g = 1.92, phi = 0.6),
                              psi_omega = c(b = 0.135, s = 0.15, g = 0.9, phi = 0.7),
                              pt_suffix = "suffix2")

sim_data_2$os$STUDYID <- factor(rep("study1", 50))
sim_data_2$os$trt <- factor(rep("TRT", 50))
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

my_mcmc_options <- jmpost:::mcmc_options(chains = 4,
                                         parallel_chains = 4,
                                         iter_warmup = 500,
                                         iter_sampling = 500,
                                         max_treedepth = 10,
                                         adapt_delta = .9,
                                         gauss_legendre = gauss_legendre())


my_mjpost <- jm_post(object = model_v3,
                     data = pre_data,
                     options = my_mcmc_options,
                     formula = ~ -1 + age + sex,
                     index_save_ind = patients_ids,
                     predictions  = seq(from = 0.001, to = 2, length = 100))

summary(my_mjpost)

