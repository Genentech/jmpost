library(jmpost)
library(mvtnorm)
library(assertthat)
library(stringr)
library(tidyverse)

my_long_mod <- exponential_long_model(functions = source_stan_part("exp_long_functions.stan"),
                                      data = source_stan_part("exp_long_data.stan"),
                                      parameters = source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)"),
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


sim_data <- simulate_os_sld()
sim_data$os$STUDYID <- rep("study@", length(sim_data$os$trt))
sim_data$os$ARM <- sim_data$os$trt

pre_data <- jmpost:::jm_data(data_sld = sim_data$sld,
                             data_os = sim_data$os,
                             vars = jmpost:::vars(longitudinal = "SLD",
                                                  os_user_id = "pt",
                                                  overall_survival_death = "event",
                                                  long_user_id = "pt",
                                                  time_survival = "time_offset",
                                                  os_study_id = "STUDYID",
                                                  os_arm = "ARM",
                                                  treatment = "trt"),
                             shared_treatment = "TRT",
                             censoring_threshold = 2.5)


patients_ids <- c(50:60)
library(rstan)
my_mcmc_options <- jmpost:::mcmc_options(chains = 1,
                                         parallel_chains = 1,
                                         iter_warmup = 10,
                                         iter_sampling = 20,
                                         max_treedepth = 10,
                                         adapt_delta = .9,
                                         gauss_legendre = gauss_legendre())

my_mjpost <- jmpost:::jm_post(object = model_v3,
                              data = pre_data,
                              options = my_mcmc_options,
                              formula = ~ -1 + age + sex,
                              index_save_ind = patients_ids,
                              predictions  = seq(from = 0.001, to = 2, length = 100))
