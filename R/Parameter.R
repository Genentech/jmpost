
#' @include generics.R
#' @include Prior.R
NULL


.Parameter <- setClass(
    Class = "Parameter",
    slots = list(
        "name" = "character",
        "prior" = "Prior"
    )
)



#' @export
Parameter <- function(prior, name) {
    .Parameter(
        prior = prior,
        name = name
    )
}



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



