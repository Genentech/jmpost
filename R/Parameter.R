#' @include generics.R
#' @include Prior.R
NULL


#' `Parameter` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`Parameter`]
#' constructors.
#'
#' @param x ([`Parameter`])\cr a prior Distribution
#' @param object ([`Parameter`])\cr a prior Distribution
#' @param ... Not Used.
#'
#' @name Parameter-Shared
#' @keywords internal
NULL


setClassUnion(name = "numeric_OR_character", c("numeric", "character"))

# Parameter-class ----

#' `Parameter`
#'
#' Stores the name, the prior distribution and the size of a parameter.
#' If `size` is a string then this indicates the name of the variable
#' within the stan data object that specifies the size of this parameter.
#'
#' @slot name (`string`)\cr of the parameter.
#' @slot prior (`Prior`)\cr for the parameter.
#' @slot size (`numeric` or `string`)\cr dimension of the parameter.
#'
#' @family Parameter
#' @exportClass Parameter
#' @export Parameter
.Parameter <- setClass(
    Class = "Parameter",
    slots = list(
        "name" = "character",
        "prior" = "Prior",
        "size" = "numeric_OR_character"
    )
)
#' @param prior (`Prior`)\cr for the parameter.
#' @param name (`string`)\cr of the parameter.
#' @param size (`numeric` or `string`)\cr dimension of the parameter.
#' @rdname Parameter-class
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


#' `Parameter` -> `StanModule`
#'
#' Converts a [`Parameter`] object to a [`StanModule`] object
#'
#' @inheritParams Parameter-Shared
#'
#' @family Parameter
#' @family as.StanModule
#' @export
as.StanModule.Parameter <- function(object, ...) {
    as.StanModule(object@prior, name = object@name)
}


#' `Parameter` -> `list`
#'
#' Converts a Parameter object to a list of parameter data values
#' for a Stan model.
#'
#' @inheritParams Parameter-Shared
#'
#' @family as_stan_list
#' @family Parameter
#' @export
as_stan_list.Parameter <- function(object, ...) {
    as_stan_list(object@prior, name = object@name)
}


#' Parameter Getter Functions
#'
#' @param x (`Paramater`) \cr A model parameter
#' @param object (`Paramater`) \cr A model parameter
#'
#' @description
#' Getter functions for the slots of a [`Parameter`] object
#' @family Parameter
#' @name Parameter-Getter-Methods
NULL

#' @describeIn Parameter-Getter-Methods The parameter's name
#' @export
names.Parameter <- function(x) x@name

#' @describeIn Parameter-Getter-Methods The parameter's initial values
#' @export
initialValues.Parameter <- function(object, ...) initialValues(object@prior)

#' @describeIn Parameter-Getter-Methods The parameter's dimensionality
#' @export
size.Parameter <- function(object) object@size


#' `Parameter` -> `Character`
#'
#' Converts a [`Parameter`] object to a character vector
#' @inheritParams Parameter-Shared
#' @family Parameter
#' @export
as.character.Parameter <- function(x, ...) {
    paste0(x@name, " ~ ", as.character(x@prior))
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "Parameter",
    definition = function(object) {
        x <- sprintf("\nParameter Object:\n   %s\n\n", as.character(object))
        cat(x)
        return(object)
    }
)
