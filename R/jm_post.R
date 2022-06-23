#' @export
#' @import cmdstanr

cmdstan_fit <- R6::R6Class("CmdStanMCMC")
setOldClass("CmdStanMCMC")


jm_post_class <- setClass("JMpost",
                          slots = c(cmdstan_fit = "CmdStanMCMC",
                                    data = "JMdata",
                                    formula = "formula",
                                    options = "mcmc_options",
                                    index_save_ind = "numeric",
                                    predictions = "numeric")
)

setGeneric("jm_post",
           function(object, data, formula, options, index_save_ind, predictions) {
  standardGeneric("jm_post")
})



setMethod("jm_post",
  signature(
    object = "JMModel",
    options = "mcmc_options"
  ),
  value = "JMpost",
  function(object, data, formula, options, index_save_ind,
           predictions) {

    inits <- replicate(1, object@inits, simplify = FALSE)

    .data <- data_prep(object = data,
                       os_formula = formula,
                       options = options,
                       index_save_ind = index_save_ind,
                       predictions = predictions)

    jm_post_class(
        cmdstan_fit = object@cmdstan_mod$sample(
            data = .data@data,
            seed = 12345,
            refresh = 250,
            init = inits,
            chains = options@chains,
            parallel_chains = options@parallel_chains,
            iter_warmup = options@iter_warmup,
            iter_sampling = options@iter_sampling,
            max_treedepth = options@max_treedepth,
            adapt_delta = options@adapt_delta
        ),
        data = .data,
        formula = formula,
        options = options,
        index_save_ind = index_save_ind,
        predictions = predictions
    )
  }
)

