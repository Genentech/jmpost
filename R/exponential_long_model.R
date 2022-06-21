
#' @export
exponential_long_model <- function(functions = long_fun,
                                   data = long_data,
                                   parameters = long_parameters,
                                   transformed_parameters = long_tr_parameters,
                                   prior = long_prior(),
                                   generated_quantities = long_gen_quantities,
                                   inits = long_inits) {
  .exponential_long_model(
    functions = functions,
    data = data,
    parameters = parameters,
    transformed_parameters = transformed_parameters,
    prior = prior,
    generated_quantities = generated_quantities,
    inits = inits
  )
}
