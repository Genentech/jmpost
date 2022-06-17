.mcmc_options <- setClass(Class = "mcmc_options",
                          slots = c(chains = "integer",
                                    parallel_chains = "integer",
                                    iter_warmup = "integer",
                                    iter_sampling = "integer",
                                    thin = "integer",
                                    max_treedepth = "integer",
                                    adapt_delta = "real"
                                    ))


McmcOptions <- function(chains = 4,
                        parallel_chains = 4,
                        iter_warmup = 200,
                        iter_sampling = 500,
                        thin = 1,
                        max_treedepth = 12,
                        adapt_delta = .9
                        ){

    .mcmc_options(chains = chains,
                  parallel_chains = parallel_chains,
                  iter_warmup = iter_warmup,
                  iter_sampling = iter_sampling,
                  thin = thin,
                  max_treedepth = max_treedepth,
                  adapt_delta = adapt_delta
                  )


}



