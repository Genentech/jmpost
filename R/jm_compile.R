

# jm_compile: Function that compiles the model
setGeneric("jm_compile", function(object) {
  standardGeneric("jm_compile")
})


setMethod("jm_compile",
  signature(object = "StanAll"),
  value = "JMModel",
  function(object) {
    cmdstanr::write_stan_file(as_charc(object),
      basename = "stan_model.stan",
      dir = paste0(system.file(package = "jmpost"), "/stanmodels")
    )

    jm_model(
      cmdstan_mod = cmdstan_model(stan_file = system.file("stanmodels/stan_model.stan",
                                                          package = "jmpost")),
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
