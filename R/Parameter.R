
#' @include generics.R
#' @include Prior.R
NULL


setClassUnion(name = "numeric_OR_character", c("numeric", "character"))

.Parameter <- setClass(
    Class = "Parameter",
    slots = list(
        "name" = "character",
        "prior" = "Prior",
        "size" = "numeric_OR_character"
    )
)



#' @export
Parameter <- function(prior, name, size = 1) {
    .Parameter(
        prior = prior,
        name = name,
        size = size
    )
}


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



#' @export
setMethod(
    f = "as.character",
    signature = "Parameter",
    definition = function(x) as(x, "character")
)



#' @export
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



#' @export 
setMethod(
    f = "names",
    signature = "Parameter",
    definition = function(x) x@name
)



#' @export 
setMethod(
    f = "initialValues",
    signature = "Parameter",
    definition = function(object) initialValues(object@prior)
)


#' @export
setMethod(
    f = "size",
    signature = "Parameter",
    definition = function(object) object@size
)

