


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

