#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include generics.R
NULL



setClassUnion("StanModule_or_Function", c("StanModule", "function"))

# TODO - docs
#' `Link`
#'
#' @slot stan (`StanModule`)\cr code containing the link specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#' @slot name (`character`)\cr display name for the link object.
#'
#' @name LinkComponent-class
#' @exportClass Link
.LinkComponent <- setClass(
    Class = "LinkComponent",
    slots = list(
        "stan" = "StanModule_or_Function",
        "parameters" = "ParameterList",
        "key" = "character"
    )
)


# TODO - docs
#' @rdname LinkComponent-class
#' @export
LinkComponent <- function(
    stan,
    parameters = ParameterList(),
    key = "",
    ...
) {
    .LinkComponent(
        stan = stan,
        key = key,
        parameters = parameters,
        ...
    )
}


# TODO - docs
#' @export
getParameters.LinkComponent <- function(object, ...) {
    object@parameters
}


#' @rdname initialValues
#' @export
initialValues.LinkComponent <- function(object, n_chains, ...) {
    initialValues(object@parameters, n_chains)
}



# TODO - docs
#' @export
as.StanModule.LinkComponent <- function(object, model = NULL, ...) {
    if (is(object@stan, "StanModule")) {
        return(object@stan)
    }
    if (is.function(object@stan)) {
        assert_that(
            is(model, "LongitudinalModel"),
            msg = "`model` must be a LongitudinalModel object"
        )
        stan <- object@stan(model)
        assert_that(
            is(stan, "StanModule"),
            msg = "The function must return a StanModule object"
        )
        return(stan)
    }
    stop("Something went wrong")
}


# TODO - docs
#' @export
as.list.LinkComponent <- function(object, ...) {
    stan <- as.StanModule(object, ...)
    as.list(stan)
}




# TODO - docs
#' @export
link_ttg <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_ttg",
        stan = linkTTG,
        parameter = ParameterList(Parameter(name = "link_ttg", prior = prior, size = 1))
    )
}

#' @export
link_dsld <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_dsld",
        stan = linkDSLD,
        parameter = ParameterList(Parameter(name = "link_dsld", prior = prior, size = 1))
    )
}

#' @export
link_identity <- function(prior = prior_normal(0, 2)) {
    LinkComponent(
        key = "link_identity",
        stan = linkIdenity,
        parameter = ParameterList(Parameter(name = "link_identity", prior = prior, size = 1))
    )
}
