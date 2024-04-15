
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include DataJoint.R
#' @include DataSurvival.R
#' @include DataLongitudinal.R
#' @include LongitudinalModel.R
#' @include SurvivalModel.R
#' @include Link.R
#' @include constants.R
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
        link = "Link",
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
    link = Link()
) {

    link <- resolvePromise(Link(link), longitudinal)

    if (length(link) > 0) {
        longitudinal <- enableLink(longitudinal)
    }

    parameters <- Reduce(
        merge,
        list(
            getParameters(longitudinal),
            getParameters(survival),
            getParameters(link)
        )
    )

    base_model <- read_stan("base/base.stan")

    stan_full <- decorated_render(
        .x = base_model,
        longitudinal = add_missing_stan_blocks(as.list(longitudinal)),
        survival = add_missing_stan_blocks(as.list(survival)),
        link = add_missing_stan_blocks(as.list(link)),
        priors = add_missing_stan_blocks(as.list(parameters))
    )
    # Unresolved Jinja code within the longitudinal / Survival / Link
    # models won't be resolved by the above call to `decorated_render`.
    # Instead they it will just be inserted into the template asis. Thus
    # we run `decorated_render` again to resolve any lingering Jinja code
    # Main example being models that don't have any Jinja code but still
    # use the `decorated_render` constants `machine_double_eps`.
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


#' `JointModel` -> `StanModule`
#'
#' Converts a [`JointModel`] object to a [`StanModule`] object
#'
#' @inheritParams JointModel-Shared
#'
#' @family JointModel
#' @family as.StanModule
#' @export
as.StanModule.JointModel <- function(object, ...) {
    object@stan
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
        # Magic constant from R/constants.R
        CMDSTAN_DEFAULT_CHAINS
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

        link_string <- as_print_string(object@link) |> pad_with_white_space()

        string <- "\nA Joint Model with:\n\n  Survival:%s\n  Longitudinal:%s\n  Link:%s\n"
        cat(sprintf(
            string,
            survival_string,
            longitudinal_string,
            link_string
        ))
    }
)
