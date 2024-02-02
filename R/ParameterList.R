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
#' @param ... (`Parameter`)\cr which parameter specifications to include.
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



# as.StanModule-ParameterList ----

#' `ParameterList` -> `StanModule`
#'
#' Converts a [`ParameterList`] object to a [`StanModule`] object
#'
#' @inheritParams ParameterList-Shared
#'
#' @family ParameterList
#' @family as.StanModule
#' @export
as.StanModule.ParameterList <- function(object, ...) {
    stan_modules <- lapply(
        object@parameters,
        as.StanModule
    )
    assert_that(
        all(vapply(stan_modules, inherits, logical(1), "StanModule"))
    )
    Reduce(merge, stan_modules)
}



#' `ParameterList` -> `list`
#'
#' Converts a ParameterList object to a list of parameter data values
#' for a Stan model.
#'
#' @inheritParams ParameterList-Shared
#'
#' @family as_stan_list
#' @family ParameterList
#' @export
as_stan_list.ParameterList <- function(object, ...) {
    stan_lists <- lapply(
        object@parameters,
        as_stan_list
    )
    assert_that(
        all(vapply(stan_lists, is.list, logical(1)))
    )
    Reduce(append, stan_lists)
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
#' @param nchains (`integer`) \cr the number of chains.
#' @name ParameterList-Getter-Methods
NULL


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter names
#' @export
names.ParameterList <- function(x) {
    vapply(x@parameters, names, character(1))
}


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter initial values
#' @export
initialValues.ParameterList <- function(object, nchains, ...) {
    x <- lapply(
        seq_len(nchains),
        \(i) {
            vals <- lapply(object@parameters, initialValues)
            name <- vapply(object@parameters, names, character(1))
            names(vals) <- name
            vals
        }
    )
    return(x)
}


#' @describeIn ParameterList-Getter-Methods The parameter-list's parameter dimensionality
#' @export
size.ParameterList <- function(object) {
    x <- lapply(object@parameters, size)
    names(x) <- names(object)
    return(x)
}


#' `ParameterList` -> Printable `Character`
#'
#' Converts [`ParameterList`] object into a printable string.
#' @inheritParams ParameterList-Shared
#' @family ParameterList
#' @keywords internal
#' @export
as_print_string.ParameterList <- function(object, ...) {
    x <- vapply(object@parameters, as.character, character(1))
    if (length(x) == 0) {
        x <- "<No Parameters>"
    }
    return(x)
}



#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "ParameterList",
    definition = function(object) {
        chrs <- as_print_string(object)
        string <- paste("   ", chrs) |> paste(collapse = "\n")
        x <- sprintf("\nParameterList Object:\n%s\n\n", string)
        cat(x)
        return(object)
    }
)
