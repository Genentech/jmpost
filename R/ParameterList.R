#' @include generics.R
#' @include Parameter.R
#' @include Prior.R
NULL


#' ParameterList-Shared
#' @param object (`ParameterList`) \cr A List of [`Parameter`] Objects.
#' @param x (`ParameterList`) \cr A List of [`Parameter`] Objects.
#' @param ... Not Used.
#' @keywords internal
#' @name ParameterList-Shared
NULL


# ParameterList-class ----

#' `ParameterList`
#'
#' This class extends the general [`list`] type for containing [`Parameter`]
#' specifications.
#'
#'
#' @param ... (`Parameter`)\cr which parameter specifications to include.
#' @slot parameters (`list`) \cr a list of [`Parameter`] objects
#' @family ParameterList
#' @export ParameterList
#' @exportClass ParameterList
.ParameterList <- setClass(
    Class = "ParameterList",
    slots = c(
        parameters = "list"
    )
)
#' @rdname ParameterList-class
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


#' `ParameterList` -> `character`
#' @description
#' Converts a [`ParameterList`] to a Stan model-block indented by 4 spaces
#' @inheritParams ParameterList-Shared
#' @family ParameterList
#' @export
as.character.ParameterList <- function(x, ...) {
    strings <- vapply(
        x@parameters,
        as.character,
        character(1)
    )
    indentation <- paste0(rep(" ", 4), collapse = "")
    strings_indented <- paste0(indentation, strings)
    paste(strings_indented, collapse = "\n")
}



# as.StanModule-ParameterList ----

#' `ParameterList` -> `StanModule`
#' @description
#' Returns a [`StanModule`] object of the model parameters.
#' @inheritParams ParameterList-Shared
#' @family ParameterList
#' @export
as.StanModule.ParameterList <- function(object) {
    x <- paste(
        "model {",
        as.character(object),
        "}",
        sep = "\n"
    )
    StanModule(x = x)
}


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

#' `ParameterList` -> `list`
#' @description
#' Returns a named list where each element of the list corresponds
#' to a Stan modelling block e.g. `data`, `model`, etc.
#' @inheritParams ParameterList-Shared
#' @family ParameterList
#' @export
as.list.ParameterList <- function(x, ...) {
    as.list(as.StanModule(x))
}




#' Parameter-List Getter Functions
#' @description
#' Getter functions for the slots of a [`ParameterList`] object
#' @inheritParams ParameterList-Shared
#' @family ParameterList
#' @name ParameterList-Getter-Methods
NULL


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter names
#' @export
names.ParameterList <- function(x) {
    vapply(x@parameters, names, character(1))
}


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter initial values
#' @export
initialValues.ParameterList <- function(object) {
    vals <- lapply(object@parameters, initialValues)
    name <- vapply(object@parameters, names, character(1))
    names(vals) <- name
    return(vals)
}


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter dimensionality
#' @export
size.ParameterList <- function(object) {
    x <- lapply(object@parameters, size)
    names(x) <- names(object)
    return(x)
}
