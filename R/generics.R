
# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))


#' `merge`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)



#' `addLink`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
setGeneric(
    name = "addLink",
    def = function(x, y, ...) standardGeneric("addLink")
)


#' `write_stan`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param file_path TODO
setGeneric(
    name = "write_stan",
    def = function(x, file_path) standardGeneric("write_stan")
)


#' `compileStanModel`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param exe_file TODO
#' @export
setGeneric(
    name = "compileStanModel",
    def = function(object, exe_file) standardGeneric("compileStanModel")
)


#' `sampleStanModel`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
#' @param exe_file TODO
#' @export
setGeneric(
    name = "sampleStanModel",
    def = function(object, ..., exe_file) standardGeneric("sampleStanModel")
)


# TODO - Convert this to a "as" function

#' `as.StanModule`
#'
#' Description - TODO
#'
#' @param object TODO
#' @param ... TODO
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
setGeneric(
    name = "getParameters",
    def = function(object, ...) standardGeneric("getParameters")
)





#' `extractVariableNames`
#'
#' @returns a `list` of variable names mapping to key variables with the
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
#' @return a `list` of initial values to be passed to the stan sampler
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
#' @return a `list` of parameter sizes
#' @keywords internal
setGeneric(
    name = "size",
    def = function(object, ...) standardGeneric("size")
)
