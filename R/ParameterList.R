#' @include generics.R
#' @include Parameter.R
#' @include Prior.R
NULL




.ParameterList <- setClass(
    Class = "ParameterList",
    slots = c(
        parameters = "list"
    )
)


#' @export
ParameterList <- function(...) {
    .ParameterList(parameters = list(...))
}



#' @export
setValidity(
    Class = "ParameterList",
    method = function(object) {
        is_parameters <- vapply(object@parameters, function(x) is(x, "Parameter"), logical(1))
        if (!all(is_parameters)) {
            return("all elements must be of class 'Parameter'")
        }
        return(TRUE)
    }
)



#' @export
setAs(
    from = "ParameterList",
    to = "character",
    def = function(from) {
        strings <- vapply(
            from@parameters,
            as.character,
            character(1)
        )
        indentation <- paste0(rep(" ", 4), collapse = "")
        strings_indented <- paste0(indentation, strings)
        paste(strings_indented, collapse = "\n")
    }
)



#' @export
setMethod(
    f = "as.character",
    signature = "ParameterList",
    definition = function(x) {
        as(x, "character")
    }
)


#' @export
setMethod(
    f = "as.StanModule",
    signature = "ParameterList",
    definition = function(object) {
        x <- paste(
            "model {",
            as(object, "character"),
            "}",
            sep = "\n"
        )
        StanModule(x = x)
    }
)


#' @export
setMethod(
    f = "merge",
    signature = c(x = "ParameterList", y = "ParameterList"),
    definition = function(x, y) {
        parameters <- append(x@parameters, y@parameters)
        do.call(ParameterList, parameters)
    }
)



#' @export
setMethod(
    f = "as.list",
    signature = "ParameterList",
    definition = function(x) {
        as.list(as.StanModule(x))
    }
)


#' @export
setMethod(
    f = "initialValues",
    signature = "ParameterList",
    definition = function(object) {
        vals <- lapply(object@parameters, initialValues)
        name <- vapply(object@parameters, names, character(1))
        names(vals) <- name
        return(vals)
    }
)


#' @export 
setMethod(
    f = "names",
    signature = "ParameterList",
    definition = function(x) {
        vapply(x@parameters, names, character(1))
    }
)



#' @export
setMethod(
    f = "size",
    signature = "ParameterList",
    definition = function(object) {
        x <- lapply(object@parameters, size)
        names(x) <- names(object)
        return(x)
    }
)

