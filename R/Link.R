#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include LinkComponent.R
NULL


# TODO - docs
.Link <- setClass(
    Class = "Link",
    slots = list(
        components = "list"
    )
)


# TODO - docs
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


# TODO - docs
#' @export
Link <- function(...) {
    components <- list(...)

    # Enable copy constructor e.g. if passed a Link just return the Link
    if (length(components) == 1 && is(components[[1]], "Link")) {
        return(components[[1]])
    }
    .Link(components = components)
}


# TODO - docs
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



# TODO - docs
#' @export
link_none <- function() {
    Link()
}




# TODO - docs
#' @export
as.list.Link <- function(object, ...) {
    as.list(as.StanModule(object, ...))
}



# TODO - docs
#' @export
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


# TODO - docs
#' @export
initialValues.Link <- function(object, ...) {
    unlist(
        lapply(object@components, initialValues),
        recursive = FALSE
    )
}

#' @export
length.Link <- function(x) {
    length(x@components)
}
