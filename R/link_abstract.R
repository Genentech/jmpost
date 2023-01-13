


.Link <- setClass(
    Class = "Link",
    slots = list(
        "stan" = "StanModule",
        "pars" = "ParameterList"
    )
)


#' @export
Link <- function(stan = StanModule(), pars = ParameterList(), ...) {
    .Link(stan = stan, pars = pars, ...)
}
