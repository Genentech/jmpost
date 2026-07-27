#' @include generics.R
#' @include Prior.R
NULL


#' `Parameter` Function Arguments
#'
#' The documentation lists all the conventional arguments for [`Parameter`]
#' constructors.
#'
#' @typed x: Parameter
#'   a prior Distribution
#' @typed object: Parameter
#'   a prior Distribution
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
#' @typed prior: Prior
#'   for the parameter.
#' @typed name: string
#'   of the parameter.
#' @typed size: "`numeric` or `string`"
#'   dimension of the parameter.
#' @rdname Parameter-class
#'
#' @returns A `Parameter` object.
#'
#' @examples
#' Parameter(prior = prior_normal(0, 1), name = "beta")
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
                return(
                    "Size must be a numeric vector or length 1 character vector"
                )
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
#'
#' @returns A `StanModule` object.
as.StanModule.Parameter <- function(object, ...) {
    merge(
        as.StanModule.ParameterDeclaration(object),
        as.StanModule(object@prior, name = object@name, size = object@size)
    )
}

#' Render Stan Parameter Constraints
#'
#' Converts a lower/upper limit vector from a [`Prior`] into a Stan declaration
#' constraint, e.g. `<lower=0.1, upper=1>`.
#'
#' @typed limits: numeric
#'   length-two vector containing lower and upper parameter limits.
#'
#' @return A length-one character vector.
#'
#' @keywords internal
render_stan_parameter_limits <- function(limits) {
    limits_names <- c("lower", "upper")
    limits <- stats::setNames(limits, limits_names)
    limits <- limits[is.finite(limits)]
    if (length(limits) == 0) {
        return("")
    }
    constraints <- paste(names(limits), limits, sep = "=", collapse = ", ")
    paste0("<", constraints, ">")
}

#' Render Stan Parameter Declaration
#'
#' Creates a Stan declaration for a sampled scalar or vector parameter.
#'
#' @typed name: string
#'   parameter name.
#' @typed size: numeric_OR_character
#'   parameter size.
#' @typed limits: numeric
#'   lower and upper parameter limits.
#'
#' @return A length-one character vector.
#'
#' @keywords internal
render_stan_parameter_declaration <- function(name, size, limits) {
    constraints <- render_stan_parameter_limits(limits)
    if (length(size) == 1 && is.numeric(size) && size == 1) {
        return(glue::glue("real{constraints} {name};"))
    }
    glue::glue("vector{constraints}[{size}] {name};")
}

#' Render Stan Constant Parameter Declaration
#'
#' Creates a Stan transformed-parameter declaration that fixes a scalar or vector
#' parameter at a data-supplied constant value.
#'
#' @typed name: string
#'   parameter name.
#' @typed size: numeric_OR_character
#'   parameter size.
#' @typed limits: numeric
#'   lower and upper parameter limits.
#' @typed is_vector: flag
#'   whether the supplied constant is a vector.
#'
#' @return A length-one character vector.
#'
#' @keywords internal
render_stan_const_declaration <- function(
    name,
    size,
    limits,
    is_vector = FALSE
) {
    constraints <- render_stan_parameter_limits(limits)
    value <- glue::glue("prior_const_{name}")
    if (length(size) == 1 && is.numeric(size) && size == 1) {
        if (is_vector) {
            value <- glue::glue("{value}[1]")
        }
        return(glue::glue("real{constraints} {name} = {value};"))
    }
    if (is_vector) {
        return(glue::glue(
            "vector{constraints}[{size}] {name} = {value};"
        ))
    }
    glue::glue(
        "vector{constraints}[{size}] {name} = rep_vector({value}, {size});"
    )
}

#' `Parameter` Declaration -> `StanModule`
#'
#' Creates only the Stan declaration for a [`Parameter`]. Sampled parameters are
#' declared in the `parameters` block; constant parameters are declared in the
#' `transformed parameters` block.
#'
#' @inheritParams Parameter-Shared
#' @param ... not used.
#'
#' @return A [`StanModule`] object.
#'
#' @export
as.StanModule.ParameterDeclaration <- function(object, ...) {
    declaration <- if (object@prior@.is_const) {
        render_stan_const_declaration(
            name = object@name,
            size = object@size,
            limits = object@prior@limits,
            is_vector = object@prior@.allow_vectors
        )
    } else {
        render_stan_parameter_declaration(
            name = object@name,
            size = object@size,
            limits = object@prior@limits
        )
    }

    block <- if (object@prior@.is_const) {
        "transformed parameters"
    } else {
        "parameters"
    }
    StanModule(glue::glue(
        "{block} {{
    {declaration}
}}"
    ))
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
#'
#' @returns A named `list` suitable for use as Stan data.
as_stan_list.Parameter <- function(object, ...) {
    as_stan_list(object@prior, name = object@name)
}


#' Parameter Getter Functions
#'
#' @typed x: Paramater
#'   A model parameter
#' @typed object: Paramater
#'   A model parameter
#' @param ... Not used.
#'
#' @description
#' Getter functions for the slots of a [`Parameter`] object
#' @family Parameter
#' @name Parameter-Getter-Methods
NULL

#' @describeIn Parameter-Getter-Methods The parameter's name
#' @export
#'
#' @returns The requested parameter property: its name, initial values, or size.
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
#'
#' @returns A character vector.
as.character.Parameter <- function(x, ...) {
    if (x@prior@.is_const) {
        return(paste0(x@name, " = ", as.character(x@prior)))
    }
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
