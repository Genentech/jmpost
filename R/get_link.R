#' @export
#' @importFrom stringr str_detect


setGeneric(name = "get_link_TTG", def = function(long_model, beta_ttg = "normal(0,5)", init = 0, ...) standardGeneric("get_link_TTG"))

setMethod(
  "get_link_TTG",
  signature = c(long_model = "ExponentialLongModel"),
  definition = function(long_model, beta_ttg = "normal(0,5)", init = 0, ...) {
    hazard_link(
      prior = beta_ttg_prior(beta_ttg),
      parameters = link_pars[str_detect(names(link_pars), "TTG")],
      arguments = link_arguments[str_detect(names(link_arguments), "TTG")],
      calculations = link_calculation[str_detect(names(link_calculation), "TTG")],
      contributions = link_log_haz[str_detect(names(link_log_haz), "TTG")],
      population_contributions = link_pop_log_haz[str_detect(names(link_pop_log_haz), "TTG")],
      inits = beta_ttg_init(init)
    )
  }
)



setGeneric(name = "get_link_DT", def = function(long_model, beta_dt = "normal(0,5)", init = 0, ...) standardGeneric("get_link_DT"))

setMethod(
  "get_link_DT",
  signature = c(long_model = "ExponentialLongModel"),
  definition = function(long_model, beta_dt = "normal(0,5)", init = 0, ...) {
    hazard_link(
      prior = beta_dt_prior(beta_dt),
      parameters = link_pars[str_detect(names(link_pars), "dt")],
      arguments = link_arguments[str_detect(names(link_arguments), "dt")],
      calculations = link_calculation[str_detect(names(link_calculation), "dt")],
      contributions = link_log_haz[str_detect(names(link_log_haz), "dt")],
      population_contributions = link_pop_log_haz[str_detect(names(link_pop_log_haz), "dt")],
      inits = beta_dt_init(init)
    )
  }
)

setGeneric(name = "get_link_Psi_Kg", def = function(long_model, beta_psi_kg = "normal(0,5)", init = 0, ...) standardGeneric("get_link_Psi_Kg"))

setMethod(
  "get_link_Psi_Kg",
  signature = c(long_model = "ExponentialLongModel"),
  definition = function(long_model, beta_psi_kg = "normal(0,5)", init = 0, ...) {
    hazard_link(
      prior = beta_psi_kg_prior(beta_psi_kg),
      parameters = link_pars[str_detect(names(link_pars), "psi_kg")],
      arguments = link_arguments[str_detect(names(link_arguments), "psi_kg")],
      calculations = link_calculation[str_detect(names(link_calculation), "psi_kg")],
      contributions = link_log_haz[str_detect(names(link_log_haz), "psi_kg")],
      population_contributions = link_pop_log_haz[str_detect(names(link_pop_log_haz), "psi_kg")],
      inits = beta_psi_kg_init(init)
    )
  }
)
