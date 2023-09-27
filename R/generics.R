# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))

# merge ----

#' `merge`
#'
#' Merge two `StanModule` or `ParameterList` objects.
#'
#' @param x first module.
#' @param y second module.
#' @param ... additional arguments.
#'
#' @export
# Needs to be S4 for multiple dispatch !
setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)


# show ----

#' Printing of Different Classes
#'
#' These methods print objects of different classes.
#'
#' @name show
#' @aliases show
#'
#' @param object what to print.
#'
#' @export
NULL



# addLink ----

#' `addLink`
#'
#' Add a link to a longitudinal model.
#'
#' @param x the longitudinal model.
#' @param y the link to be added.
#' @param ... additional arguments.
#'
#' @export
# Needs to be S4 for multiple dispatch !
setGeneric(
    name = "addLink",
    def = function(x, y, ...) standardGeneric("addLink")
)



# write_stan ----

#' `write_stan`
#'
#' Write the Stan code for a Stan module.
#'
#' @param object the module.
#' @param file_path (`string`)\cr output file.
#'
#' @export
write_stan <- function(object, file_path) {
    UseMethod("write_stan")
}

# compileStanModel ----

#' `compileStanModel`
#'
#' Compile the Stan module.
#'
#' @param object the module.
#'
#' @export
compileStanModel <- function(object) {
    UseMethod("compileStanModel")
}


# sampleStanModel ----

#' `sampleStanModel`
#'
#' Sample from a Stan Module.
#'
#' @param object the module.
#' @param ... additional arguments.
#'
#' @export
sampleStanModel <- function(object, ...) {
    UseMethod("sampleStanModel")
}


# as.StanModule ----

#' `as.StanModule`
#'
#' Convert a [`Link`] or [`ParameterList`] into a [`StanModule`].
#'
#' @param object what to convert.
#'
#' @keywords internal
as.StanModule <- function(object) {
    UseMethod("as.StanModule")
}


# getParameters ----

#' `getParameters`
#'
#' Obtain the parameters from a [`StanModel`].
#'
#' @param object where to obtain the parameters from.
#'
#' @keywords internal
getParameters <- function(object) {
    UseMethod("getParameters")
}


# extractVariableNames ----

#' Extract Mapping to Standardised Variable Names
#'
#' @description
#' Extract a `list` that maps the variable names in a user-defined
#' `data.frame` to standardised values.
#'
#' @param object the data object.
#' @family extractVariableNames
#' @keywords internal
extractVariableNames <- function(object) {
    UseMethod("extractVariableNames")
}



# initialValues ----

#' `initialValues`
#'
#' Obtain the `list` of initial values to be passed to the Stan sampler.
#'
#' @param object where to get the initial values from.
#'
#' @keywords internal
initialValues <- function(object) {
    UseMethod("initialValues")
}


# size ----

#' `size`
#'
#' Obtain the `list` of parameter sizes.
#'
#' @param object where to get the parameter sizes from.
#'
#' @keywords internal
size <- function(object) {
    UseMethod("size")
}

# longitudinal ----

#' `longitudinal`
#'
#' Obtain the longitudinal fit samples from [`JointModelSamples`].
#'
#' @param object samples to extract the longitudinal fits from.
#' @param ... additional options.
#'
#' @export
longitudinal <- function(object, ...) {
    UseMethod("longitudinal")
}


# generateQuantities ----

#' `generateQuantities`
#'
#' Obtain the generated quantities from a Stan Model.
#'
#' @param object object to obtain generated quantities from
#' @param ... additional options.
#'
#' @export
generateQuantities <- function(object, ...) {
    UseMethod("generateQuantities")
}
