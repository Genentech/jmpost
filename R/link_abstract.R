


.Link <- setClass(
    Class = "Link",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList"
    )
)


#' @export
Link <- function(stan = StanModule(), parameters = ParameterList(), ...) {
    .Link(stan = stan, parameters = parameters, ...)
}



setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "Link"),
    definition = function(x, y, ...) {
        x@stan <- merge(x@stan, y@stan)
        x@parameters <- merge(x@parameters, y@parameters)
        x
    }
)

