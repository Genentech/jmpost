#' @include generics.R
#' @include Parameter.R
#' @include Prior.R
NULL

# ParameterList-class ----

#' `ParameterList`
#'
#' This class extends the general [`list`] type for containing [`Parameter`]
#' specifications.
#'
#' @exportClass LinkGSF
.ParameterList <- setClass(
    Class = "ParameterList",
    slots = c(
        parameters = "list"
    )
)

# ParameterList-constructors ----

#' @rdname ParameterList-class
#'
#' @param ... (`Parameter`)\cr which parameter specifications to include.
#'
#' @export
ParameterList <- function(...) {
    .ParameterList(parameters = list(...))
}

# ParameterList-validity ----

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

# coerce-ParameterList,character ----

#' @rdname as.character
#'
#' @name coerce-ParameterList-character-method
#' @aliases coerce,ParameterList,character-method
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

# as.character-ParameterList ----

#' @rdname as.character
setMethod(
    f = "as.character",
    signature = "ParameterList",
    definition = function(x) {
        as(x, "character")
    }
)

# as.StanModule-ParameterList ----

#' @rdname as.StanModule
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

# merge-ParameterList,ParameterList ----

#' @rdname merge
setMethod(
    f = "merge",
    signature = c(x = "ParameterList", y = "ParameterList"),
    definition = function(x, y) {
        parameters <- append(x@parameters, y@parameters)
        do.call(ParameterList, parameters)
    }
)

# as.list-ParameterList ----

#' @rdname as.list
setMethod(
    f = "as.list",
    signature = "ParameterList",
    definition = function(x) {
        as.list(as.StanModule(x))
    }
)

# initialValues-ParameterList ----

#' @rdname initialValues
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

# names-ParameterList ----

#' @rdname names
setMethod(
    f = "names",
    signature = "ParameterList",
    definition = function(x) {
        vapply(x@parameters, names, character(1))
    }
)

# size-ParameterList ----

#' @rdname size
setMethod(
    f = "size",
    signature = "ParameterList",
    definition = function(object) {
        x <- lapply(object@parameters, size)
        names(x) <- names(object)
        return(x)
    }
)
