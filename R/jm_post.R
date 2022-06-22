#' @export
#' @import cmdstanr

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
           predictions = seq(from = 0.001, to = 2, length = 100)) {

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
        data_list = .data@data,
        vars_map = data@vars,
        cmdstan_mod = object@cmdstan_mod,
        prior = object@prior,
        model = object@model,
        functions = object@functions,
        data = object@data,
        parameters = object@parameters,
        transformed_parameters = object@transformed_parameters,
        generated_quantities = object@generated_quantities,
        includes = object@includes,
        inits = object@inits
    )
  }
)

