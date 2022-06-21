#' @export

.mcmc_options <- setClass(Class = "mcmc_options",
                          slots = c(chains = "numeric",
                                    parallel_chains = "numeric",
                                    iter_warmup = "numeric",
                                    iter_sampling = "numeric",
                                    max_treedepth = "numeric",
                                    adapt_delta = "numeric",
                                    gauss_legendre = "gauss_legendre"
                          ))


mcmc_options <- function(chains = 1,
                         parallel_chains = 1,
                         iter_warmup = 200,
                         iter_sampling = 500,
                         max_treedepth = 12,
                         adapt_delta = .9,
                         gauss_legendre = gauss_legendre()
){

    .mcmc_options(chains = chains,
                  parallel_chains = parallel_chains,
                  iter_warmup = iter_warmup,
                  iter_sampling = iter_sampling,
                  max_treedepth = max_treedepth,
                  adapt_delta = adapt_delta,
                  gauss_legendre = gauss_legendre
    )


}



