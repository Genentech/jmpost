#' @export

setMethod("summary",
          signature = c(object = "JMpost"),
          definition = function(object){
              object@cmdstan_fit$summary()
          })
