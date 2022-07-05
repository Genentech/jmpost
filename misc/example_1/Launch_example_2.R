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
                                      generated_quantities = "")

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
model_v3 <- jm_compile(my_jmpost)

my_vars <- jmpost::vars()
pre_data <- jm_data(sld = sld,
                    os = osd_final,
                    vars = my_vars,
                    shared_treatment = "Atezo",
                    censoring_threshold = .0005)

library(rstan)
my_mcmc_options <- jmpost:::mcmc_options(chains = 1,
                                parallel_chains = 1,
                                iter_warmup = 10,
                                iter_sampling = 20,
                                max_treedepth = 10,
                                adapt_delta = .9,
                                gauss_legendre = gauss_legendre())


patients_ids <- c(1,840 )

my_mjpost <- jm_post(object = model_v3,
                     data = pre_data,
                     options = my_mcmc_options,
                     formula = ~ -1 + BECOG + CLOGNLR + CLOGCRP + CALBU + CLIVER,
                     index_save_ind = patients_ids,
                     predictions = seq(from = 0.001, to = 2, length = 100))


plot(KM(my_mjpost))
my_post_check <- jm_post_check(my_mjpost)

res <- my_mjpost@cmdstan_fit$summary()



draws_df <- my_mjpost@cmdstan_fit$draws(format = "df")


all.equal(ttg(draws_df$`psi_ks[1]`,
              draws_df$`psi_kg[1]`,
              draws_df$`psi_phi[1]`),
          as.numeric(my_mjpost@cmdstan_fit$draws("save_ind_ttg", format = "draws_matrix")[,1., drop = T]),
          tolerance = .01)


ind_uncond_surv <- my_mjpost@cmdstan_fit$draws("save_ind_unconditional_survival", format = "draws_matrix")
str(ind_uncond_surv)

