
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include DataJoint.R
#' @include DataSurvival.R
#' @include DataLongitudinal.R
#' @include LongitudinalModel.R
#' @include SurvivalModel.R
#' @include Link.R
NULL


#' Re-used documentation for `JointModel-Shared`
#'
#' @param object ([`JointModel`]) \cr Joint model specification.
#' @param x ([`JointModel`]) \cr Joint model specification.
#' @param ... Not Used.
#'
#' @name JointModel-Shared
#' @keywords internal
NULL

setClassUnion("LongitudinalModel_OR_NULL", c("LongitudinalModel", "NULL"))
setClassUnion("Link_OR_NULL", c("Link", "NULL"))
setClassUnion("SurvivalModel_OR_NULL", c("SurvivalModel", "NULL"))

# JointModel-class ----

#' Joint Model Object and Constructor Function
#'
#' @slot longitudinal ([`LongitudinalModel`] or `NULL`)\cr the longitudinal model.
#' @slot survival ([`SurvivalModel`] or `NULL`)\cr the survival model.
#' @slot link (`Link`)\cr the link.
#' @slot stan (`StanModule`)\cr code containing the joint model specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#'
#' @family JointModel
#' @export JointModel
#' @exportClass JointModel
.JointModel <- setClass(
    Class = "JointModel",
    slots = list(
        longitudinal = "LongitudinalModel_OR_NULL",
        survival = "SurvivalModel_OR_NULL",
        link = "Link_OR_NULL",
        stan = "StanModule",
        parameters = "ParameterList"
    )
)

#' @param longitudinal ([`LongitudinalModel`] or `NULL`)\cr the longitudinal model.
#' @param survival ([`SurvivalModel`] or `NULL`)\cr the survival model.
#' @param link (`Link`)\cr the link.
#' @rdname JointModel-class
JointModel <- function(
    longitudinal = NULL,
    survival = NULL,
    link = NULL
) {
    longitudinal_linked <- addLink(longitudinal, link)

    parameters <- merge(
        getParameters(longitudinal_linked),
        getParameters(survival)
    )

    base_model <- paste0(read_stan("base/base.stan"), collapse = "\n")

    stan_full <- decorated_render(
        .x = base_model,
        longitudinal = add_missing_stan_blocks(as.list(longitudinal_linked)),
        survival = add_missing_stan_blocks(as.list(survival)),
        priors = as.list(parameters),
        link_none = class(link)[[1]] == "LinkNone" | is.null(link)
    )
    # Resolve any lingering references from longitudinal / survival code
    # that haven't yet been rendered
    stan_full <- decorated_render(.x = stan_full)

    stan_complete <- merge(
        StanModule("base/functions.stan"),
        StanModule(stan_full)
    )

    .JointModel(
        longitudinal = longitudinal,
        survival = survival,
        link = link,
        stan = stan_complete,
        parameters = parameters
    )
}


#' `JointModel` -> `character`
#'
#' Renders a [`JointModel`] object to a stan program
#'
#' @inheritParams JointModel-Shared
#' @family JointModel
#' @export
as.character.JointModel <- function(x, ...) {
    as.character(x@stan)
}


# write_stan-JointModel ----

#' @rdname write_stan
#' @export
write_stan.JointModel <- function(object, file_path) {
    fi <- file(file_path, open = "w")
    writeLines(as.character(object), fi)
    close(fi)
}


# compileStanModel-JointModel ----

#' @rdname compileStanModel
#' @export
compileStanModel.JointModel <- function(object) {
    stanObject <- object@stan
    stanObject@generated_quantities <- ""
    x <- compileStanModel(stanObject)
    invisible(x)
}


# sampleStanModel-JointModel ----

#' @rdname sampleStanModel
#'
#' @param data (`DataJoint` or `list`)\cr input data.
#' @export
sampleStanModel.JointModel <- function(object, data, ...) {

    assert_class(data, "DataJoint")

    if (!is.null(object@survival)) {
        assert_that(
            !is.null(data@survival),
            msg = "`DataSurvival` can't be missing if a `SurvivalModel` has been specified"
        )
    }
    if (!is.null(object@longitudinal)) {
        assert_that(
            !is.null(data@longitudinal),
            msg = "`DataLongitudinal` can't be missing if a `LongitudinalModel` has been specified"
        )
    }

    args <- list(...)

    args[["data"]] <- append(
        as_stan_list(data),
        as_stan_list(object@parameters)
    )

    args[["chains"]] <- if ("chains" %in% names(args)) {
        args[["chains"]]
    } else {
        4
    }

    initial_values <- if ("init" %in% names(args)) {
        args[["init"]]
    } else {
        initialValues(object, n_chains = args[["chains"]])
    }

    args[["init"]] <- ensure_initial_values(
        initial_values,
        args[["data"]],
        object@parameters
    )

    model <- compileStanModel(object)

    results <- do.call(
        model$sample,
        args
    )

    .JointModelSamples(
        model = object,
        data = data,
        results = results
    )
}


#' Ensure that initial values are correctly specified
#'
#' @param initial_values (`list`)\cr A list of lists containing the initial values
#' must be 1 list per desired chain. All elements should have identical names
#' @param data (`list`)\cr specifies the size to expand each of our initial values to be.
#' That is elements of size 1 in `initial_values` will be expanded to be the same
#' size as the corresponding element in `data` by broadcasting the value.
#' @param parameters ([`ParameterList`])\cr the parameters object
#'
#' @details
#' This function is mostly a thin wrapper around `expand_initial_values` to
#' enable easier unit testing.
#'
#' @keywords internal
ensure_initial_values <- function(initial_values, data, parameters) {
    if (is.function(initial_values)) {
        return(initial_values)
    }

    assert_class(data, "list")
    assert_class(parameters, "ParameterList")
    assert_class(initial_values, "list")

    values_sizes <- size(parameters)
    values_sizes_complete <- replace_with_lookup(
        values_sizes,
        data
    )
    lapply(
        initial_values,
        expand_initial_values,
        sizes = values_sizes_complete
    )
}



#' @rdname initialValues
#' @export
initialValues.JointModel <- function(object, n_chains, ...) {
    initialValues(object@parameters, n_chains)
}


pad_with_white_space <- function(x, pad = 4) {
    padding <- paste0(rep(" ", each = pad), collapse = "")
    x_sep <- x |>
        strsplit(split = "\n") |>
        unlist()
    x_padded <- paste(padding, x_sep) |>
        paste(collapse = "\n")
    return(x_padded)
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "JointModel",
    definition = function(object) {
        survival_string <- if (is.null(object@survival)) {
            "\n     Not Specified\n"
        } else {
            as_print_string(object@survival) |> pad_with_white_space()
        }

        longitudinal_string <- if (is.null(object@longitudinal)) {
            "\n     Not Specified\n"
        } else {
            as_print_string(object@longitudinal) |> pad_with_white_space()
        }

        link_string <- if (is.null(object@link) || inherits(object@link, "LinkNone")) {
            "\n     No Link\n"
        } else {
            as_print_string(object@link) |> pad_with_white_space()
        }

        string <- "\nA Joint Model with:\n\n  Survival:%s\n  Longitudinal:%s\n  Link:%s\n"
        cat(sprintf(
            string,
            survival_string,
            longitudinal_string,
            link_string
        ))
    }
)
