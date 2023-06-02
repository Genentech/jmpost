
# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))


#' `merge`
#'
#' Merge two Stan modules.
#'
#' @param x first module.
#' @param y second module.
#' @param ... additional arguments.
#'
#' @export
setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)



#' `addLink`
#'
#' Add a link to a longitudinal model.
#'
#' @param x the longitudinal model.
#' @param y the link to be added.
#' @param ... additional arguments.
#'
#' @export
setGeneric(
    name = "addLink",
    def = function(x, y, ...) standardGeneric("addLink")
)


#' `write_stan`
#'
#' Write the Stan code for a Stan module.
#'
#' @param object the module.
#' @param file_path (`string`)\cr output file.
#'
#' @export
setGeneric(
    name = "write_stan",
    def = function(object, file_path) standardGeneric("write_stan")
)


#' `compileStanModel`
#'
#' Compile the Stan module.
#'
#' @param object the module.
#' @param exe_file (`string`)\cr output file.
#'
#' @export
setGeneric(
    name = "compileStanModel",
    def = function(object, exe_file) standardGeneric("compileStanModel")
)


#' `sampleStanModel`
#'
#' Sample from a Stan Module.
#'
#' @param object the module.
#' @param ... additional arguments.
#' @param exe_file (`string`)\cr output file.
#'
#' @export
setGeneric(
    name = "sampleStanModel",
    def = function(object, ..., exe_file) standardGeneric("sampleStanModel")
)


#' `as.StanModule`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
#'
#' @keywords internal
setGeneric(
    name = "as.StanModule",
    def = function(object, ...) standardGeneric("as.StanModule")
)


#' `getParameters`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
#'
#' @keywords internal
setGeneric(
    name = "getParameters",
    def = function(object, ...) standardGeneric("getParameters")
)



#' `extractVariableNames`
#'
#' @param object the module.
#'
#' @returns A `list` of variable names mapping to key variables with the
#' source-data stored inside the data class.
#'
#' @keywords internal
setGeneric(
    name = "extractVariableNames",
    def = function(object, ...) standardGeneric("extractVariableNames")
)



#' `initialValues`
#'
#' @param object TODO
#' @param ... TODO
#' @returns A `list` of initial values to be passed to the Stan sampler.
#'
#' @keywords internal
setGeneric(
    name = "initialValues",
    def = function(object, ...) standardGeneric("initialValues")
)




#' `size`
#'
#' @param object TODO
#' @param ... TODO
#'
#' @returns A `list` of parameter sizes.
#' @keywords internal
setGeneric(
    name = "size",
    def = function(object, ...) standardGeneric("size")
)


#' `longitudinal`
#'
#' @param object (`JointModelSamples`)\cr samples to extract the longitudinal fits from.
#' @param ... not used.
#'
#' @returns The longitudinal fit samples.
#' @export
setGeneric(
    name = "longitudinal",
    def = function(object, ...) standardGeneric("longitudinal")
)

#' Plotting Methods for Different Classes
#'
#' These plot methods visualize various objects.
#'
#' @name autoplot
#' @aliases autoplot
#'
#' @param object (`LongitudinalSamples`)\cr what to plot.
#' @param ... Other arguments passed to plotting methods.
#'
#' @export
NULL
