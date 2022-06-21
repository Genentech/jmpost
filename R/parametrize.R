#' @export



setGeneric("parametrize", function(osmod, link) {
  standardGeneric("parametrize")
})


setMethod(
  "parametrize",
  signature(osmod = "temp_stan_os", link = "HazardLink"),
  function(osmod, link) {
      newOS <- stan_os(data = osmod@data,
                       model = osmod@model,
                       includes = osmod@includes)

    newOS@functions <- str_replace_all(
      string = osmod@functions,
      pattern = "<link_arguments>",
      replacement = link@arguments
    ) %>%
      str_replace(
        pattern = "<link_calculations>",
        replacement = link@calculations
      ) %>%
      str_replace(
        pattern = "<link_log_hazard_contribution>",
        replacement = link@contributions
      ) %>%
      str_replace(
        pattern = "<link_arguments_as_par>",
        replacement = str_remove_all(link@arguments, "real")
      ) %>%
      str_replace(
        pattern = "<Link_pop_log_haz>",
        replacement = link@population_contributions
      )

    newOS@prior <- append(osmod@prior, link@prior)
    newOS@inits <- append(osmod@inits, link@inits)

    newOS@parameters <- str_replace(
      string = osmod@parameters,
      pattern = "<link_parameters>",
      replacement = link@parameters
    )

    newOS@transformed_parameters <- str_replace(
      string = osmod@transformed_parameters,
      pattern = "<link_log_surv>",
      replacement = str_remove_all(link@arguments, "real")
    ) %>%
      str_replace(
        pattern = "<link_log_lik>",
        replacement = str_remove_all(link@arguments, "real")
      )

    newOS@generated_quantities <- str_replace_all(
      string = osmod@generated_quantities,
      pattern = "<link_arguments_as_par>",
      replacement = str_remove_all(link@arguments, "real")
    )


    newOS
  }
)
