source("C:/Users/kazantg1/Desktop/jmstan/daniel/MercierSabanes2022/data_25Feb2022.R", chdir = TRUE)
osd <- osd %>%
    dplyr::mutate(
        CLIVER = LIVER - 1,
        CLOGNLR = log(NLR) - log(3.8),
        CLOGCRP = log(CRP) - log(17.4),
        CALBU = ALBU - 39
    )

osd_final <-
    osd %>%
    dplyr::mutate(
        AYR = ifelse(STUDYID == "WO39613", 0, AYR),
        DEATH = ifelse(STUDYID == "WO39613", 0, DEATH)
    )

library(jmpost)


my_long_mod <- exponential_long_model(functions = source_stan_part("exp_long_functions.stan"),
                                      data = source_stan_part("exp_long_data.stan"),
                                      parameters = source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)"),
                                      generated_quantities = "",
                                      inits = long_inits)

my_os_mod <- temp_stan_os(functions = source_stan_part("os_functions.stan"),
                          data = source_stan_part("os_data.stan"),
                          parameters = source_stan_part("os_parameters.stan"),
                          transformed_parameters = source_stan_part("os_transformed_parameters.stan"),
                          prior = os_prior(),
                          generated_quantities = source_stan_part("os_generated_quantities.stan"),
                          inits = os_inits)

library(stringr)
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
pre_data <- jm_data(data_sld = sld,
                    data_os = osd_final,
                    vars = jmpost::vars(),
                    shared_treatement = "Atezo",
                    censoring_threshold = 2.5)
library(rstan)
my_mcmc_options <- mcmc_options(chains = 1,
                                parallel_chains = 1,
                                iter_warmup = 10,
                                iter_sampling = 20,
                                max_treedepth = 10,
                                adapt_delta = .9,
                                gauss_legendre = gauss_legendre())


patients_ids <- c(771, 772, 773, 774, 775, 776, 777, 778, 779 )

my_mjpost <- jm_post(object = model_v3,
                     data = pre_data,
                     options = my_mcmc_options,
                     formula = ~ -1 + BECOG + CLOGNLR + CLOGCRP + CALBU + CLIVER,
                     index_save_ind = patients_ids,
                     predictions  = seq(from = 0.001, to = 2, length = 100))

library(survival)
plot(KM(my_mjpost))
# scaled_brier(my_mjpost, pre_data)

library(bayesplot)
my_mjpost@cmdstan_fit$summary()
mcmc_hist(my_mjpost@cmdstan_fit$draws("beta_ttg"))
my_mjpost@cmdstan_fit$diagnostic_summary()

