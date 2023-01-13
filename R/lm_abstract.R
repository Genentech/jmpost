

.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    slots = list(
        "stan" = "StanModule",
        "pars" = "ParameterList"
    )
)


#' @export
LongitudinalModel <- function(stan = StanModule(), pars = ParameterList(), ...) {
        
    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )
    
    .LongitudinalModel(
        stan = merge(base_long, stan),
        pars = pars,
        ...
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


#' @export
setMethod(
    f = "as.list",
    signature = c("LongitudinalModel"),
    definition = function(x) {
        as.list(x@stan)
    }
)


