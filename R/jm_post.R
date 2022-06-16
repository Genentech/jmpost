
setGeneric("jm_post", function(object, options) {
  standardGeneric("jm_post")
})



setMethod("jm_post",
  signature(
    object = "JMModel",
    options = "mcmc_options"
  ),
  value = "JMpost",
  function(object, options) {
    inits <- replicate(1, object@inits, simplify = FALSE)
    jm_post_class(
      cmdstan_fit = object@cmdstan_mod$sample(
        data = options@data,
        seed = options@seed,
        refresh = options@refresh,
        init = inits,
        save_latent_dynamics = options@save_latent_dynamics,
        output_dir = options@output_dir,
        output_basename = options@output_basename,
        sig_figs = options@sig_figs,
        chains = options@chains,
        parallel_chains = options@parallel_chains,
        chain_ids = options@chain_ids,
        threads_per_chain = options@threads_per_chain,
        opencl_ids = options@opencl_ids,
        iter_warmup = options@iter_warmup,
        iter_sampling = options@iter_sampling,
        save_warmup = options@save_warmup,
        thin = options@thin,
        max_treedepth = options@max_treedepth,
        adapt_engaged = options@adapt_engaged,
        adapt_delta = options@adapt_delta,
        step_size = options@step_size,
        window = options@window,
        fixed_param = options@fixed_param,
        show_messages = options@show_messages,
        diagnostics = options@diagnostics
      ),
      data_list = options@data,
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
