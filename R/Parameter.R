#' @include generics.R
#' @include Prior.R
NULL

setClassUnion(name = "numeric_OR_character", c("numeric", "character"))

# Parameter-class ----

#' `Parameter`
#'
#' Stores the name, the prior distribution and the size of a parameter.
#'
#' @slot name (`string`)\cr of the parameter.
#' @slot prior (`Prior`)\cr for the parameter.
#' @slot size (`numeric` or `string`)\cr dimension of the parameter.
#'
#' @exportClass Parameter
.Parameter <- setClass(
    Class = "Parameter",
    slots = list(
        "name" = "character",
        "prior" = "Prior",
        "size" = "numeric_OR_character"
    )
)

# Parameter-constructors ----

#' @rdname Parameter-class
#'
#' @param prior (`Prior`)\cr for the parameter.
#' @param name (`string`)\cr of the parameter.
#' @param size (`numeric` or `string`)\cr dimension of the parameter.
#'
#' @export
Parameter <- function(prior, name, size = 1) {
    .Parameter(
        prior = prior,
        name = name,
        size = size
    )
}

# Parameter-validity ----

setValidity(
    Class = "Parameter",
    method = function(object) {
        if (!length(object@name) == 1) {
            return("Name must be a length 1 character vector")
        }
        if (is.character(object@size)) {
            if (!length(object@size) == 1) {
                return("Size must be a numeric vector or length 1 character vector")
            }
        }
        return(TRUE)
    }
)

# as.character-Parameter ----

#' @rdname as.character
setMethod(
    f = "as.character",
    signature = "Parameter",
    definition = function(x) as(x, "character")
)

# coerce-Parameter,character ----

#' @rdname as.character
#'
#' @name coerce-Parameter-character-method
#' @aliases coerce,Parameter,character-method
setAs(
    from = "Parameter",
    to = "character",
    def = function(from) {
        if (as.character(from@prior) =="") {
             return("")
        }
        glue::glue("{name} ~ {dist}", name = from@name, dist = as(from@prior, "character"))
    }
)

# names-Parameter ----

#' @rdname names
setMethod(
    f = "names",
    signature = "Parameter",
    definition = function(x) x@name
)

# initialValues-Parameter ----

#' @rdname initialValues
setMethod(
    f = "initialValues",
    signature = "Parameter",
    definition = function(object) initialValues(object@prior)
)

# size-Parameter ----

#' @rdname size
setMethod(
    f = "size",
    signature = "Parameter",
    definition = function(object) object@size
)
