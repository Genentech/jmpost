


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
