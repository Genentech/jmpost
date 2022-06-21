#' @export

setGeneric("jm_post", function(object, data, formula, options) {
  standardGeneric("jm_post")
})



setMethod("jm_post",
  signature(
    object = "JMModel",
    options = "mcmc_options"
  ),
  value = "JMpost",
  function(object, data, formula, options) {

    inits <- replicate(1, object@inits, simplify = FALSE)

    .data <- data_prep(data = data,
                       os_formula = formula,
                       options = options)

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
        thin = options@thin,
        max_treedepth = options@max_treedepth,
        adapt_delta = options@adapt_delta
      ),
      data_list = .data@data,
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
