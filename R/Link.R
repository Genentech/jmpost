#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include LinkComponent.R
NULL


#' `Link` Function Arguments
#'
#' This exists just to contain all the common arguments for [`Link`] methods.
#'
#' @param x ([`Link`])\cr a link object.
#' @param object ([`Link`])\cr a link object.
#' @param ... Not Used.
#'
#' @name Link-Shared
#' @keywords internal
NULL




#' `Link`
#'
#' @slot components (`list`)\cr a list of [`LinkComponent`] objects.
#'
#' @param ... ([`LinkComponent`])\cr an arbitrary number of link components.
#'
#' @description
#' Simple container class to enable the use of multiple link components in a joint model.
#' Note that the constructor of this object is idempotent e.g. `Link(Link(x)) == Link(x)`
#'
#' @examples
#' Link(
#'     link_dsld(),
#'     link_ttg()
#' )
#'
#' @family Link
#' @name Link-class
#' @exportClass Link
.Link <- setClass(
    Class = "Link",
    slots = list(
        components = "list"
    )
)

#' @rdname Link-class
#' @export
Link <- function(...) {
    components <- list(...)

    # Enable copy constructor e.g. if passed a Link just return the Link
    if (length(components) == 1 && is(components[[1]], "Link")) {
        return(components[[1]])
    }
    .Link(components = components)
}



setValidity(
    Class = "Link",
    method = function(object) {
        if (length(object@components) == 0) {
            return(TRUE)
        }
        for (component in object@components) {
            if (!is(component, "LinkComponent")) {
                return("Link components must be of class `LinkComponent`.")
            }
        }
        return(TRUE)
    }
)




#' `Link` -> `StanModule`
#'
#' Converts a [`Link`] object to a [`StanModule`] object
#'
#' @inheritParams Link-Shared
#'
#' @family Link
#' @family as.StanModule
#' @export
as.StanModule.Link <- function(object, ...) {

    if (length(object@components) == 0) {
        return(StanModule("base/link_none.stan"))
    }

    keys <- vapply(
        object@components,
        function(x) x@key,
        character(1)
    )

    base_stan <- StanModule(
        decorated_render(
            .x = paste(read_stan("base/link.stan"), collapse = "\n"),
            items = as.list(keys)
        )
    )

    stan_list <- lapply(
        object@components,
        as.StanModule,
        ...
    )

    stan <- Reduce(
        merge,
        append(base_stan, stan_list)
    )
    return(stan)
}




#' `Link` -> `list`
#'
#' @inheritParams Link-Shared
#'
#' @description
#' Returns a named list where each element of the list corresponds
#' to a Stan modelling block e.g. `data`, `model`, etc.
#'
#' @family Link
#' @export
as.list.Link <- function(x, ...) {
    as.list(as.StanModule(x, ...))
}



#' @export
#' @rdname getParameters
getParameters.Link <- function(object, ...) {
    parameters_list <- lapply(
        object@components,
        getParameters,
        ...
    )
    Reduce(
        merge,
        parameters_list
    )
}


#' @rdname initialValues
#' @export
initialValues.Link <- function(object, ...) {
    unlist(
        lapply(object@components, initialValues),
        recursive = FALSE
    )
}


#' `Link` -> `list`
#'
#' @inheritParams Link-Shared
#'
#' @description
#' Returns the number of link components within the [`Link`] object
#'
#' @family Link
#' @export
length.Link <- function(x) {
    length(x@components)
}


#' @export
as_print_string.Link <- function(object, ...) {
    if (length(object) == 0) {
        return("\nNo Link")
    }
    paste(
        c(
            "\nLink with the following components/parameters:",
            paste0(
                "    ",
                vapply(object@components, as_print_string, character(1))
            )
        ),
        collapse = "\n"
    )
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "Link",
    definition = function(object) {
        cat(paste0(as_print_string(object), "\n"))
    }
)
