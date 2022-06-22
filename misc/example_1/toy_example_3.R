my_long_mod <- exponential_long_model(functions = source_stan_part("exp_long_functions.stan"),
                                      data = source_stan_part("exp_long_data.stan"),
                                      parameters = source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)"),
                                      generated_quantities = "",
                                      inits = long_inits)

my_os_mod <- stan_os(functions = source_stan_part("os_functions.stan"),
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
my_link <- merge_link(link1 = my_link_dt, link2 = my_link_ttg, additive = TRUE)

# Fill in the stanos with the link
my_final_os <- parametrize(osmod = my_os_mod, link = my_link)

# name it jm_merge eg because it is special. All all stuff here and convert lists to character already
my_jmpost <- jm_complete(my_long_mod, my_final_os)
library(cmdstanr)
model_v3 <- jm_compile(my_jmpost)
