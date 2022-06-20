

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
      dir = paste0(getwd(), "/inst/stanmodels")
    )

    jm_model(
      cmdstan_mod = cmdstan_model(stan_file = file.path(
        dirname(getwd()),
        "jmpost/inst/stanmodels/stan_model.stan"
      )),
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
