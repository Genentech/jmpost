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
#' @typed object: JointModel
#'   Joint model specification.
#' @typed x: JointModel
#'   Joint model specification.
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
#' @slot parameters (`ParameterList`)\cr the parameter declaration and prior specification.
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
        parameters = "ParameterList"
    )
)

#' @typed longitudinal: "`LongitudinalModel` or `NULL`"
#'   the longitudinal model.
#' @typed survival: "`SurvivalModel` or `NULL`"
#'   the survival model.
#' @typed link: Link
#'   the link.
#' @rdname JointModel-class
#'
#' @returns A `JointModel` object.
#'
#' @examples
#' JointModel(LongitudinalGSF(), SurvivalWeibullPH(), linkDSLD())
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

    .JointModel(
        longitudinal = longitudinal,
        survival = survival,
        link = link,
        parameters = parameters
    )
}


#' @export
#'
#' @returns A `StanModule` object containing the generated-quantities code.
enableGQ.JointModel <- function(object, ...) {
    merge(
        enableGQ(object@survival, ...),
        enableGQ(object@longitudinal, ...)
    )
}


#' `JointModel` -> `StanModule`
#'
#' Converts a [`JointModel`] object to a [`StanModule`] object
#'
#' @inheritParams JointModel-Shared
#' @family JointModel
#' @family as.StanModule
#' @export
#'
#' @returns A `StanModule` object.
as.StanModule.JointModel <- function(object, ...) {
    base_model <- read_stan("base/base.stan")

    longitudinal <- add_missing_stan_blocks(as.list(object@longitudinal))
    survival <- add_missing_stan_blocks(as.list(object@survival))
    link <- add_missing_stan_blocks(as.list(object@link))
    priors <- add_missing_stan_blocks(as.list(object@parameters))

    # Evaluate the same prior statements in generated quantities, including
    # hierarchical distributions and explicit Jacobian adjustments. The two
    # base likelihood increments are already represented by log_lik.
    prior_code <- paste(
        longitudinal$model, link$model, survival$model, priors$model,
        collapse = "\n"
    )
    prior_code <- gsub(
        "target\\s*\\+=\\s*sum\\((long_obvs_log_lik|os_subj_log_lik)\\);",
        "", prior_code
    )
    log_prior <- stan_prior_density(prior_code)

    stan_full <- decorated_render(
        .x = base_model,
        longitudinal = longitudinal,
        survival = survival,
        link = link,
        priors = priors,
        log_prior = log_prior,
        has_os_submodel = !is.null(object@survival),
        has_long_submodel = !is.null(object@longitudinal)
    )
    # Unresolved Jinja code within the longitudinal / Survival / Link
    # models won't be resolved by the above call to `decorated_render`.
    # Instead they it will just be inserted into the template asis. Thus
    # we run `decorated_render` again to resolve any lingering Jinja code
    # Main example being models that don't have any Jinja code but still
    # use the `decorated_render` constants `machine_double_eps`.
    stan_full <- decorated_render(.x = stan_full)

    merge(
        StanModule("base/functions.stan"),
        StanModule(stan_full)
    )
}


#' Render Stan Prior Density Accumulation
#'
#' Converts the continuous prior statements used by `jmpost` into explicit
#' log-density evaluations for the generated quantities block.
#'
#' @typed code: string
#'   Stan model-block contents containing only prior contributions. The caller
#'   must remove likelihood contributions before calling this function.
#'
#' @details
#' Sampling statements of the form `x ~ distribution(arguments);` become
#' `lprior += distribution_lpdf(x | arguments);`. Distributions with no
#' arguments, such as `std_normal()`, omit the vertical bar. Explicit
#' `target +=` statements are redirected to `lprior`, retaining adjustments
#' such as transformed-prior Jacobians. Line comments starting with `//` are
#' removed; surrounding braced control flow and local declarations are retained.
#'
#' The model block already evaluates these priors for sampling. Re-evaluating
#' them in generated quantities deliberately adds work once per saved draw to
#' keep the extra logging calculation out of HMC leapfrog and gradient
#' evaluations. The original sampling statements also let Stan omit terms that
#' are constant with respect to the parameters. Computing `lprior` in
#' transformed parameters and adding it to `target` would avoid the repeated
#' prior evaluation when saving draws, but would evaluate the full logged
#' density at every leapfrog step. Likelihood outputs reuse the existing
#' likelihood evaluations and only add aggregation.
#'
#' For truncation with scalar bounds (`T[lower, upper]`, including either
#' one-sided form), the output subtracts the log normalising probability for
#' each element of `x`. Lower-only truncation uses the log complementary CDF,
#' upper-only truncation uses the log CDF, and two-sided truncation uses
#' `log_diff_exp()` of the two log CDFs. Distribution arguments are accessed
#' elementwise, broadcasting scalars and length-one vectors. The loop is guarded
#' so that an empty vector contributes zero. Parameter-dependent normalisation,
#' such as that of a bounded horseshoe prior, is therefore retained.
#'
#' This helper handles the well-formed continuous prior syntax emitted by the
#' package templates, rather than arbitrary Stan programs. Sampling statements
#' must end in semicolons; control-flow bodies must be braced. Block comments,
#' discrete distributions and brace-delimited array literals are unsupported.
#' The caller must declare and initialise `lprior` and include the
#' `prior_density_size()` and `prior_density_element()` overloads from
#' `base/functions.stan` when truncation is present. The original model code is
#' not modified by this conversion.
#'
#' @returns A length-one character vector containing Stan statements that
#'   accumulate the joint log prior in `lprior`.
#' @seealso [stan_density_arguments()]
#' @keywords internal
stan_prior_density <- function(code) {
    code <- gsub("//[^\n]*", "", code)
    code <- stringr::str_replace_all(
        code,
        paste0(
            "([^;{}]+?)\\s*~\\s*([a-zA-Z][a-zA-Z0-9_]*)",
            "\\s*\\(([^;]*)\\)\\s*(?:T\\s*\\[[^;]*\\])?\\s*;"
        ),
        function(statement) {
            parts <- stringr::str_match(
                statement,
                paste0(
                    "(?s)^(.*?)\\s*~\\s*([a-zA-Z][a-zA-Z0-9_]*)",
                    "\\s*\\((.*?)\\)\\s*(?:T\\s*\\[([^;]*)\\])?\\s*;$"
                )
            )
            arguments <- trimws(parts[, 4])
            vapply(seq_len(nrow(parts)), function(i) {
                value <- trimws(parts[i, 2])
                distribution <- parts[i, 3]
                args <- arguments[i]
                density <- paste0(
                    "\n    lprior += ", distribution, "_lpdf(", value,
                    if (nzchar(args)) paste0(" | ", args) else "", ");"
                )
                if (is.na(parts[i, 5])) return(density)

                bounds <- trimws(strsplit(parts[i, 5], ",", fixed = TRUE)[[1]])
                # strsplit drops a trailing empty upper bound.
                lower <- bounds[1]
                upper <- if (length(bounds) == 2) bounds[2] else ""
                scalar_args <- vapply(
                    stan_density_arguments(args),
                    function(x) {
                        paste0("prior_density_element(", x, ", prior_j)")
                    },
                    character(1)
                )
                scalar_args <- if (length(scalar_args)) {
                    paste0(" | ", paste(scalar_args, collapse = ", "))
                } else {
                    ""
                }
                cdf <- function(bound, suffix) {
                    paste0(distribution, suffix, "(", bound, scalar_args, ")")
                }
                normalisation <- if (!nzchar(lower)) {
                    cdf(upper, "_lcdf")
                } else if (!nzchar(upper)) {
                    cdf(lower, "_lccdf")
                } else {
                    paste0(
                        "log_diff_exp(", cdf(upper, "_lcdf"), ", ",
                        cdf(lower, "_lcdf"), ")"
                    )
                }
                paste0(
                    density, "\n    if (prior_density_size(", value, ") > 0) {",
                    "\n        for (prior_j in 1:prior_density_size(", value,
                    ")) {\n            lprior -= ", normalisation, ";",
                    "\n        }\n    }"
                )
            }, character(1))
        }
    )
    gsub("target\\s*\\+=", "lprior +=", code)
}


#' Split a Stan Density Argument List
#'
#' Splits a comma-separated Stan argument list at its outermost level, retaining
#' commas inside nested function calls and indexing expressions.
#'
#' @typed code: string
#'   Contents of a Stan function's argument list, without its enclosing
#'   parentheses. Empty or whitespace-only input is allowed.
#'
#' @details
#' Leading and trailing whitespace is removed from each argument; internal
#' whitespace, argument order and repeated arguments are preserved. Nesting is
#' determined by parentheses and square brackets, including multidimensional
#' indexes. The input is assumed to contain well-formed expressions with
#' balanced delimiters. String literals, comments and brace-delimited array
#' literals are not parsed.
#'
#' @returns A character vector with one element per argument, or `character(0)`
#'   for an empty or whitespace-only argument list.
#' @seealso [stan_prior_density()]
#' @keywords internal
stan_density_arguments <- function(code) {
    if (!nzchar(trimws(code))) return(character())
    characters <- strsplit(code, "", fixed = TRUE)[[1]]
    depth <- cumsum(
        as.integer(characters %in% c("(", "[")) -
            as.integer(characters %in% c(")", "]"))
    )
    separators <- which(characters == "," & depth == 0)
    starts <- c(1, separators + 1)
    ends <- c(separators - 1, length(characters))
    trimws(substring(code, starts, ends))
}


#' `JointModel` -> `character`
#'
#' Renders a [`JointModel`] object to a stan program
#'
#' @inheritParams JointModel-Shared
#' @family JointModel
#' @export
#'
#' @returns A character vector.
as.character.JointModel <- function(x, ...) {
    as.character(as.StanModule(x))
}


# write_stan-JointModel ----

#' @rdname write_stan
#' @export
write_stan.JointModel <- function(object, destination, ...) {
    if (is_connection(destination)) {
        return(writeLines(as.character(object), con = destination))
    }
    fi <- file(destination, open = "w")
    writeLines(as.character(object), con = fi)
    close(fi)
    invisible()
}


# compileStanModel-JointModel ----

#' @rdname compileStanModel
#' @export
compileStanModel.JointModel <- function(object) {
    object |>
        as.StanModule() |>
        compileStanModel() |>
        invisible()
}


# sampleStanModel-JointModel ----

#' @rdname sampleStanModel
#'
#' @typed data: "`DataJoint` or `list`"
#'   input data.
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
        as_stan_list(data, object),
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
#' @typed initial_values: list
#'   A list of lists containing the initial values
#'   must be 1 list per desired chain. All elements should have identical names
#' @typed data: list
#'   specifies the size to expand each of our initial values to be.
#'   That is elements of size 1 in `initial_values` will be expanded to be the same
#'   size as the corresponding element in `data` by broadcasting the value.
#' @typed parameters: ParameterList
#'   the parameters object
#'
#' @details
#' This function is mostly a thin wrapper around `expand_initial_values` to
#' enable easier unit testing.
#'
#' @keywords internal
#'
#' @returns A list of initial values, one element per chain.
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


#' @rdname getRandomEffectsNames
#' @export
getRandomEffectsNames.JointModel <- function(object, ...) {
    if (is.null(object@longitudinal)) {
        return(NULL)
    }
    getRandomEffectsNames(object@longitudinal)
}
