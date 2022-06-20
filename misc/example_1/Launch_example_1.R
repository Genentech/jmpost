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

my_path <- paste0("C:/Users/kazantg1/Desktop/jmpost/R/")             # set your path
 <- list.files(my_path, "*.R$")  # locate all .R files
library(tidyverse)
source("C:/Users/kazantg1/Desktop/jmpost/R/stan_all.R")
map(paste0(my_path, source_files), source)


temp_source_stan_part <- function(filename) {
    absolute_filename <- paste0(
        "C:/Users/kazantg1/Desktop/jmpost/inst/stanparts/",
        filename
    )
    readChar(absolute_filename, file.info(absolute_filename)$size)
}



my_long_mod <- exponential_long_model(functions = temp_source_stan_part("exp_long_functions.stan"),
                                      data = temp_source_stan_part("exp_long_data.stan"),
                                      parameters = temp_source_stan_part("exp_long_parameters.stan"),
                                      transformed_parameters = temp_source_stan_part("exp_long_transformed_parametes.stan"),
                                      prior = long_prior(sigma = "lognormal(-1.7,0.8)"),
                                      generated_quantities = "",
                                      inits = long_inits)

my_os_mod <- stan_os(functions = temp_source_stan_part("os_functions.stan"),
                     data = temp_source_stan_part("os_data.stan"),
                     parameters = temp_source_stan_part("os_parameters.stan"),
                     transformed_parameters = temp_source_stan_part("os_transformed_parameters.stan"),
                     prior = os_prior(),
                     generated_quantities = temp_source_stan_part("os_generated_quantities.stan"),
                     inits = os_inits)


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
pre_data <- jm_data(data_sld = sld,
                    data_os = osd_final,
                    vars = vars())
library(rstan)
pre_data_try <- data_prep(pre_data)


my_mjpost <- jm_post(object = model_v3,
                     data = pre_data,
                     options = mcmc_options())

