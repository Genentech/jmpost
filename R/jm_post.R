
setGeneric("jm_post", function(object, data, ...) {
  standardGeneric("jm_post")
})



setMethod("jm_post",
          signature(
            object = "JMModel",
            data = "JMdata"
          ),
          value = "JMpost",
          function(object, data, ...) {
            .data <- data_prep(data)

            inits <- replicate(1, object@inits, simplify = FALSE)
            jm_post_class(
              cmdstan_fit = object@cmdstan_mod$sample(
                data = .data@data,
                init = inits,
                ...
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
