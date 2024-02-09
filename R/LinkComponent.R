#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
NULL



#' `Link`
#'
#' @slot stan (`StanModule`)\cr code containing the link specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#' @slot name (`character`)\cr display name for the link object.
#'
#' @exportClass Link
.LinkComponent <- setClass(
    Class = "Link",
    slots = list(
        "name" = "character",
        "stan" = "StanModule",
        "parameters" = "ParameterList",
        "parameter_name" = "character",
        "contribution_fname" = "character"
    )
)



#' @rdname Link-class
#'
#' @inheritParams stanmodel_arguments
#' @param ... additional arguments passed to the constructor.
#'
#' @export
LinkComponent <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    parameter_name = "",
    contribution_fname = "",
    ...
) {
    .LinkComponent(
        name = name,
        stan = stan,
        parameters = parameters,
        parameter_name = parameter_name,
        contribution_fname = contribution_fname,
        ...
    )
}


#' @rdname addLink
setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkComponent"),
    definition = function(x, y, ...) {
        x@stan <- merge(x@stan, y@stan)
        x@parameters <- merge(x@parameters, y@parameters)
        x
    }
)

# initialValues-Link ----

#' @rdname initialValues
#' @export
initialValues.LinkComponent <- function(object, n_chains, ...) {
    initialValues(object@parameters, n_chains)
}


link_ttg <- function(prior) {
    function(model) {
        link_files <- getLinkFiles(model)
        if (!"link_ttg" %in% link_files) {
            stop(sprintf(
                "The %s model does not support the TTG link.",
                model@name
            ))
        }
        LinkComponent(
            stan = StanModule(link_files[["link_ttg"]]),
            parameter = ParameterList(Parameter(name = "link_ttg", prior = prior, size = 1)),
            parameter_name = "link_ttg",
            contribution_fname = "link_ttg_contribution"
        )
    }
}


link_dsld <- function(prior) {
    function(model) {
        link_files <- getLinkFiles(model)
        if (!"link_dsld" %in% link_files) {
            stop(sprintf(
                "The %s model does not support the dsld link.",
                model@name
            ))
        }
        LinkComponent(
            stan = StanModule(link_files[["link_dsld"]]),
            parameter = ParameterList(Parameter(name = "link_dsld", prior = prior, size = 1)),
            parameter_name = "link_dsld",
            contribution_fname = "link_dsld_contribution"
        )
    }
}


link_identity <- function(prior) {
    function(model) {
        link_files <- getLinkFiles(model)
        if (!"link_identity" %in% link_files) {
            stop(sprintf(
                "The %s model does not support the Identity link.",
                model@name
            ))
        }
        LinkComponent(
            stan = StanModule(link_files[["link_identity"]]),
            parameter = ParameterList(Parameter(name = "link_identity", prior = prior, size = 1)),
            parameter_name = "link_identity",
            contribution_fname = "link_identity_contribution"
        )
    }
}
