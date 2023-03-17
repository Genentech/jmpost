
# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))


setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)


setGeneric(
    name = "addLink",
    def = function(x, y, ...) standardGeneric("addLink")
)


setGeneric(
    name = "write_stan",
    def = function(x, file_path) standardGeneric("write_stan")
)


#' @export
setGeneric(
    name = "compileStanModel",
    def = function(object, exe_file) standardGeneric("compileStanModel")
)


#' @export
setGeneric(
    name = "sampleStanModel",
    def = function(object, ..., exe_file) standardGeneric("sampleStanModel")
)


setGeneric(
    name = "as.StanModule",
    def = function(object, ...) standardGeneric("as.StanModule")
)


setGeneric(
    name = "getParameters",
    def = function(object, ...) standardGeneric("getParameters")
)


setGeneric(
    name = "getInits",
    def = function(object, ...) standardGeneric("getInits")
)


#' @export
setGeneric(
    name = "as.StanData",
    def = function(object, ...) standardGeneric("as.StanData")
)
