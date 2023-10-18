#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
NULL

# Link-class ----

#' `Link`
#'
#' @slot stan (`StanModule`)\cr code containing the link specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#' @slot name (`character`)\cr display name for the link object.
#'
#' @exportClass Link
.Link <- setClass(
    Class = "Link",
    slots = list(
        "name" = "character",
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
Link <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    ...
) {
    .Link(
        name = name,
        stan = stan,
        parameters = parameters,
        ...
    )
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

#' @export
as_print_string.Link <- function(object, ...) {
    string <- sprintf(
        "\n%s Link with parameters:\n%s\n\n",
        object@name,
        paste("   ", as_print_string(object@parameters)) |> paste(collapse = "\n")
    )
    return(string)
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "Link",
    definition = function(object) {
        cat(as_print_string(object))
    }
)
