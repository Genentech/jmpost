


.StanModel <- setClass(
    Class = "StanModel",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList"
    )
)

StanModel <- function(stan, parameters, ...) {
    .StanModel(
        stan = stan,
        parameters = parameters,
        ...
    )
}


setMethod(
    f = "getParameters",
    signature = "StanModel",
    definition = function(object) {
        object@parameters
    }
)


#' @export
setMethod(
    f = "as.list",
    signature = c("StanModel"),
    definition = function(x) {
        as.list(x@stan)
    }
)


