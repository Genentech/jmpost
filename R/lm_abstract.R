

.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    contains = "StanModel"
)


#' @export
LongitudinalModel <- function(stan = StanModule(), parameters = ParameterList(), ...) {
        
    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )
    
    .LongitudinalModel(
        StanModel(
            stan = merge(base_long, stan),
            parameters = parameters,
            ...
        )
    )
}


# TODO - Document that you can override this if adding a no-link is not simple
setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkNone"),
    definition = function(x, y, ...) {
        x
    }
)


