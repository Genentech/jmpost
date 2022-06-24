#' Exponential longitudinal model creator for joint modeling analysis
#'
#' @param functions Stan object with the functions of the longitudinal model
#' @param data Stan object with the data of the longitudinal model
#' @param parameters Stan object with the parameters of the longitudinal model
#' @param transformed_parameters Stan object with the transformed parameters of the longitudinal model
#' @param prior list with the priors of the longitudinal model
#' @param generated_quantities Stan object with the generated quantities of the longitudinal model
#' @param inits list
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
