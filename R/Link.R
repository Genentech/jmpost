#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
NULL

# Link-class ----

#' `Link`
#'
#' @slot stan (`StanModule`)\cr code containing the link specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#'
#' @exportClass Link
.Link <- setClass(
    Class = "Link",
    slots = list(
        "stan" = "StanModule",
        "parameters" = "ParameterList"
    )
)

# Link-constructors ----

#' @rdname Link-class
#'
#' @inheritParams stanmodel_arguments
#' @param ... additional arguments passed to the constructor.
#'
#' @export
Link <- function(stan = StanModule(),
                 parameters = ParameterList(),
                 ...) {
    .Link(stan = stan, parameters = parameters, ...)
}

# addLink-LongitudinalModel,Link ----

#' @rdname addLink
setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "Link"),
    definition = function(x, y, ...) {
        x@stan <- merge(x@stan, y@stan)
        x@parameters <- merge(x@parameters, y@parameters)
        x
    }
)

# initialValues-Link ----

#' @rdname initialValues
initialValues.Link <- function(object) {
    initialValues(object@parameters)
}


# Link-as.StanModule ----

#' @rdname as.StanModule
as.StanModule.Link <- function(object) {
    object@stan
}
