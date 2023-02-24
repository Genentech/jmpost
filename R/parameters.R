


.Parameter <- setClass(
    Class = "Parameter",
    slots = list(
        "name" = "character",
        "prior" = "Prior",
        "init" = "numeric"  # TODO - Likely needs to be more flexible than this
    )
)

#' @export
Parameter <- function(prior=NULL, init=NULL, name = NULL) {
    args <- list()
    args[["prior"]] <- prior
    args[["name"]] <- name
    args[["init"]] <- init
    do.call(.Parameter, args)
}

#' @export
setMethod(
    f = "as.character",
    signature = "Parameter",
    definition = function(x) {
        assertthat::assert_that(
            !is.null(x@name) & !length(x@name) == 0,
            msg = "Parameters must be given a name!"
        )
        # TODO - This doesn't currently work
        assertthat::assert_that(
            !is.null(x@prior) & !length(x@prior) == 0,
            msg = "Parameters must be given a name!"
        )
        glue::glue("{name} ~ {dist}", name = x@name, dist = as.character(x@prior))
    }
)


.ParameterList <- setClass(
    Class = "ParameterList",
    slots = c(
        parameters = "list"
    )
)

#' @export
ParameterList <- function(...) {
    pars <- list(...)
    if (length(pars) == 0) {
        return(.ParameterList())
    }
    pars_coerced <- priors2parameters(pars)
    pars_named <- fix_parameter_names(pars_coerced)
    assertthat::assert_that(
        all(vapply(pars_named, function(x) is(x, "Parameter"), logical(1))),
        msg = "all elements must be of class 'Parameter'"
    )
    .ParameterList(parameters = pars_named)
}


#' @export
setMethod(
    f = "as.character",
    signature = "ParameterList",
    definition = function(x, indent = 4) {
        strings <- vapply(
            x@parameters,
            as.character,
            character(1)
        )
        indentation <- paste0(rep(" ", indent), collapse = "")
        strings_indented <- paste0(indentation, strings)
        paste(strings_indented, collapse = "\n")
    }
)


#' @export
setMethod(
    f = "as.StanModule",
    signature = "ParameterList",
    definition = function(object) {
        x <- paste(
            "model {",
            as.character(object, indent = 4),
            "}",
            sep = "\n"
        )
        StanModule(x = x)
    }
)


#' @export
setMethod(
    f = "merge",
    signature = c(x = "ParameterList", y = "ParameterList"),
    definition = function(x, y) {
        parameters <- append(x@parameters, y@parameters)
        do.call(ParameterList, parameters)
    }
)



#' @export
setMethod(
    f = "as.list",
    signature = "ParameterList",
    definition = function(x) {
        as.list(as.StanModule(x))
    }
)


#' @export
setMethod(
    f = "getInits",
    signature = "ParameterList",
    definition = function(object) {
        vals <- vapply(object@parameters, function(x) x@init, numeric(1))
        name <- vapply(object@parameters, function(x) x@name, character(1))
        names(vals) <- name
        return(vals)
    }
)


fix_parameter_names <- function(x) {
    x_names <- names(x)
    if (is.null(x_names)) {
        x_names <- rep("", length(x))
    }
    for (idx in seq_along(x)) {
        item <- x[[idx]]
        idx_name <- x_names[[idx]]
        if (idx_name != "") {
            item@name <- idx_name
        }
        x[[idx]] <- item
    }
    names(x) <- NULL
    return(x)
}


priors2parameters <- function(x) {
    for (idx in seq_along(x)) {
        item <- x[[idx]]
        if (is(item, "Prior")) {
            x[[idx]] <- Parameter(prior = item)
        }
    }
    return(x)
}

