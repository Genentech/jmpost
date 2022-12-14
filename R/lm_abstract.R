


.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    slots = list(
        "stan" = "StanModule"
    )
)


#' @export
LongitudinalModel <- function(stan = StanModule(), ...) {
    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )
    .LongitudinalModel(
        stan = merge(base_long, stan),
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
