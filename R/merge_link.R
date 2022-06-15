setGeneric("merge_link", def = function(link1, link2, additive = TRUE) {
  standardGeneric("merge_link")
})

# 3: can merge all StanAll objects with each other
setMethod("merge_link",
  signature(link1 = "HazardLink", link2 = "HazardLink"),
  value = "HazardLink",
  def = function(link1, link2, additive) {
    if (additive) {
      sign <- "+"
    } else {
      sign <- "*"
    }

    hazard_link(
      functions = paste0(link1@functions, link2@functions),
      data = paste0(link1@data, link2@data),
      prior = append(link1@prior, link2@prior),
      parameters = paste0(link1@parameters, link2@parameters),
      transformed_parameters = paste0(link1@transformed_parameters, link2@transformed_parameters),
      generated_quantities = paste0(link1@generated_quantities, link2@generated_quantities),
      arguments = paste0(link1@arguments, link2@arguments),
      calculations = paste0(link1@calculations, link2@calculations),
      contributions = paste0(link1@contributions, sign, link2@contributions),
      inits = append(link1@inits, link2@inits),
      population_contributions = paste0(link1@population_contributions, link2@population_contributions)
    )
  }
)
