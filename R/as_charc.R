# as_charc: Generic function for converting a stanAll object to a character vector
setGeneric("as_charc", function(object) {
  standardGeneric("as_charc")
})

# h_bracket: function for adding brackets at the sections of the stan model as character
h_bracket <- function(x) {
  paste("{\n", x, "\n}\n\n", sep = "")
}


setMethod("as_charc",
  signature(object = "StanAll"),
  value = "character",
  function(object) {
    c(paste(
      "functions ", h_bracket(object@functions),
      "data ", h_bracket(object@data),
      "parameters", h_bracket(object@parameters),
      "transformed parameters ", h_bracket(object@transformed_parameters),
      "model ", h_bracket(object@model),
      "generated quantities", h_bracket(object@generated_quantities),
      sep = ""
    ))
  }
)
