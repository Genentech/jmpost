
#' Initial values preperation
#' @param object
#' @param data
#' @export


setGeneric("inits_prep", def = function(object, data) {
  standardGeneric("inits_prep")
})

# provide object@inits from the beginning.
setMethod("inits_prep",
  signature(
    object = "JMModel",
    data = "JMdata"
  ),
  value = "JMModel",
  function(object, data) {
    for (i in 1:length(object@inits)) {
      if (is.function(object@inits[[i]])) {
          object@inits[[i]] <- do.call(
              object@inits[[i]],
          list(data)
        )
      }
    }

    object
  }
)
