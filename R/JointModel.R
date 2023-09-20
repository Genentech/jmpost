
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
NULL

# JointModel-class ----

#' `JointModel`
#'
#' @slot stan (`StanModule`)\cr code containing the joint model specification.
#' @slot parameters (`ParameterList`)\cr the parameter specification.
#'
#' @exportClass JointModel
.JointModel <- setClass(
    Class = "JointModel",
    slots = list(
        stan = "StanModule",
        parameters = "ParameterList"
    )
)

# JointModel-constructors ----

#' @rdname JointModel-class
#'
#' @param longitudinal (`LongitudinalModel` or `NULL`)\cr the longitudinal model.
#' @param survival (`SurvivalModel` or `NULL`)\cr the survival model.
#' @param link (`Link`)\cr the link.
#'
#' @export
JointModel <- function(longitudinal = NULL,
                       survival = NULL,
                       link = NULL) {

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

    full_plus_funs <- merge(
        StanModule("base/functions.stan"),
        StanModule(stan_full)
    )

    .JointModel(
        stan = full_plus_funs,
        parameters = parameters
    )
}

# as.character-JointModel ----

#' @rdname as.character
setMethod(
    f = "as.character",
    signature = "JointModel",
    definition = function(x) {
        as.character(x@stan)
    }
)

# write_stan-JointModel ----

#' @rdname write_stan
setMethod(
    f = "write_stan",
    signature = "JointModel",
    definition = function(object, file_path) {
        fi <- file(file_path, open = "w")
        writeLines(as.character(object), fi)
        close(fi)
    }
)

# compileStanModel-JointModel ----

#' @rdname compileStanModel
setMethod(
    f = "compileStanModel",
    signature = "JointModel",
    definition = function(object) {
        x <- compileStanModel(object@stan)
        invisible(x)
    }
)

# sampleStanModel-JointModel ----

#' @rdname sampleStanModel
#'
#' @param data (`DataJoint` or `list`)\cr input data.
setMethod(
    f = "sampleStanModel",
    signature = "JointModel",
    definition = function(object, data, ...) {

        args <- list(...)
        args[["data"]] <- as.list(data)

        if (!"init" %in% names(args)) {
            values_initial <- initialValues(object)
            values_sizes <- size(object@parameters)
            values_sizes_complete <- replace_with_lookup(values_sizes, args[["data"]])
            values_initial_expanded <- expand_initial_values(values_initial, values_sizes_complete)
            args[["init"]] <- function() values_initial_expanded
        }

        stanObject <- object@stan
        stanObject@generated_quantities <- ""
        model <- compileStanModel(stanObject)
        results <- do.call(model$sample, args)

        .JointModelSamples(
            model = object,
            data = data,
            results = results
        )
    }
)

# initialValues-JointModel ----

#' @rdname initialValues
setMethod(
    f = "initialValues",
    signature = "JointModel",
    definition = function(object) {
        initialValues(object@parameters)
    }
)
