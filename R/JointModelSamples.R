#' @include JointModel.R
#' @include SurvivalQuantities.R
NULL

setOldClass("CmdStanMCMC")

# JointModelSamples-class ----

#' `JointModelSamples`
#'
#' Contains samples from a [`JointModel`].
#'
#' @slot model ([`JointModel`])\cr the model that the samples were drawn from.
#' @slot data ([`DataJoint`])\cr the data that the model was fitted on.
#' @slot results ([`cmdstanr::CmdStanMCMC`])\cr the STAN samples.
#'
#' @aliases JointModelSamples
#' @export
#'
#' @returns A `JointModelSamples` object.
#'
#' @examples
#' \dontrun{
#' # JointModelSamples objects are returned by sampleStanModel().
#' fit <- sampleStanModel(JointModel(LongitudinalGSF()), data = joint_data)
#' }
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "DataJoint",
        results = "CmdStanMCMC"
    )
)


#' @rdname generateQuantities
#' @typed generator: QuantityGenerator
#'   object that specifies which subjects and time points
#'   to calculate the quantities at
#' @typed type: character
#'   type of quantities to be generated, must be either "survival" or
#'   "longitudinal".
#' @export
generateQuantities.JointModelSamples <- function(object, generator, type, ...) {
    data <- as_stan_list(object@data, object@model) |>
        append(as_stan_list(object@model@parameters)) |>
        append(as_stan_list(
            generator,
            data = object@data,
            model = object@model
        ))

    stanobj <- as.StanModule(object, generator = generator, type = type)
    model <- compileStanModel(stanobj)

    devnull <- utils::capture.output(
        results <- model$generate_quantities(
            data = data,
            fitted_params = object@results
        )
    )
    return(results)
}


#' `JointModelSamples` -> `StanModule`
#'
#' Converts a `JointModelSamples` object into a `StanModule` object ensuring
#' that the resulting `StanModule` object is able to generate post sampling
#' quantities.
#'
#' @inheritParams generateQuantities
#' @export
#'
#' @returns A `StanModule` object.
as.StanModule.JointModelSamples <- function(object, generator, type, ...) {
    assert_that(
        is(generator, "QuantityGenerator"),
        length(type) == 1,
        type %in% c("survival", "longitudinal")
    )

    longitudinal_gq_population_data <- if (type == "longitudinal") {
        gq_population_stan_data(
            generator,
            model = object@model@longitudinal
        )
    } else {
        list(declarations = "", data = list())
    }

    quant_stanobj <- read_stan("base/quantities.stan") |>
        decorated_render(
            include_gq_longitudinal_idv = (type == "longitudinal") &
                is(generator, "QuantityGeneratorSubject"),
            include_gq_longitudinal_pop = (type == "longitudinal") &
                is(generator, "QuantityGeneratorPopulation"),
            include_gq_survival_idv = (type == "survival") &
                is(generator, "QuantityGeneratorSubject"),
            include_gq_survival_pred = (type == "survival") &
                is(generator, "QuantityGeneratorPrediction"),
            longitudinal_gq_population_data =
                longitudinal_gq_population_data$declarations
        ) |>
        StanModule()

    stanobj <- Reduce(
        merge,
        list(
            as.StanModule(object@model),
            enableGQ(
                object@model,
                generator = generator,
                type = type
            ),
            quant_stanobj
        )
    )
    stanobj
}


#' `JointModelSamples` -> Printable `Character`
#'
#' Converts [`JointModelSamples`] object into a printable string.
#'
#' @typed object: JointModelSamples
#'   samples as drawn from a [`JointModel`].
#' @family JointModelSamples
#' @typed indent: numeric
#'   how much white space to prefix the print string with.
#' @keywords internal
#' @export
#'
#' @returns A character vector suitable for printing.
as_print_string.JointModelSamples <- function(object, indent = 1, ...) {
    sizes <- vapply(
        cmdstanr::as.CmdStanMCMC(object)$metadata()[["stan_variable_sizes"]],
        \(x) {
            if (length(x) == 1 && x == 1) {
                return("")
            }
            paste0("[", paste(x, collapse = ", "), "]")
        },
        character(1)
    )
    variable_string <- paste0(
        "        ",
        cmdstanr::as.CmdStanMCMC(object)$metadata()[["stan_variables"]],
        sizes
    )
    template <- c(
        "JointModelSamples Object with:",
        "",
        "    # of samples per chain = %d",
        "    # of chains            = %d",
        "",
        "    Variables:",
        variable_string[order(variable_string)]
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n"),
        cmdstanr::as.CmdStanMCMC(object)$metadata()$iter_sampling,
        cmdstanr::as.CmdStanMCMC(object)$num_chains()
    )
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "JointModelSamples",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)


#' @rdname as.CmdStanMCMC
#'
#' @returns The underlying `cmdstanr::CmdStanMCMC` object.
as.CmdStanMCMC.JointModelSamples <- function(object, ...) {
    return(object@results)
}


#' Save a `JointModelSamples` object to a file.
#'
#' This function is just a wrapper around `saveRDS` that saves the object to a file
#' ensuring that all of the Stan samples are correctly stored. Note that as
#' `cmdstanr` objects store their samples as a csv file the samples may be lost
#' if you call `saveRDS` directly on the object.
#'
#' @typed object: JointModelSamples
#'   the object to save.
#' @typed file: character
#'   the file to save the object to.
#' @typed ...: ANY
#'   additional arguments to [`saveRDS`].
#'
#' @family saveObject
#'
#' @export
#'
#' @returns Invisibly returns `NULL` after saving the object.
saveObject.JointModelSamples <- function(object, file, ...) {
    object@results$draws()
    try(object@results$sampler_diagnostics(), silent = TRUE)
    try(object@results$init(), silent = TRUE)
    try(object@results$profiles(), silent = TRUE)
    saveRDS(object, file, ...)
}

#' Summarise posterior covariate coefficients
#'
#' Extracts posterior summaries of survival and longitudinal covariate
#' coefficients from a fitted joint model. Uses the existing [stats::coef()]
#' generic. Survival-only and longitudinal-only fits are also supported.
#'
#' Covariate labels come from the fitted design matrices, including factor
#' contrasts and interactions. Longitudinal coefficients are grouped by their
#' predictor, such as `mu_g`, `mu_s`, or `omega_g`; all covariate predictors
#' reported by [covariates.LongitudinalModel()] are included.
#'
#' Coefficients remain on their fitted predictor scales. No exponentiation or
#' other transformation is applied. Intercepts, association-link coefficients,
#' and other model parameters are not included. An error is raised if there
#' are no covariate coefficients or a required coefficient was not sampled.
#'
#' @typed object: JointModelSamples
#'   samples from a fitted joint model.
#' @param ... Summary functions or named arguments passed to the underlying
#'   `cmdstanr::CmdStanMCMC` object's `$summary()` method, which uses
#'   [posterior::summarise_draws()]. Supply a custom quantile function to
#'   change the credible interval (see examples). Do not supply `variables`:
#'   the method selects the coefficient variables automatically.
#' @returns A tibble with one row per covariate coefficient, in design-matrix
#'   order (survival first, then longitudinal predictors). The columns are:
#'   * `component`: `"survival"` or `"longitudinal"`.
#'   * `predictor`: the longitudinal predictor name, or `NA` for survival.
#'   * `covariate`: the corresponding design-matrix column name.
#'   * `variable`: the original indexed Stan variable name.
#'   * Summary columns returned by `$summary()`. By default these include
#'     `mean`, `median`, `sd`, `mad`, `q5`, `q95`, `rhat`, `ess_bulk`, and
#'     `ess_tail`. The default intervals are 90% credible intervals.
#'
#' @family JointModelSamples
#' @seealso [shrinkage.JointModelSamples()]
#' @export
#' @examples
#' \dontrun{
#' # fit is a JointModelSamples object returned by sampleStanModel().
#' coef(fit)
#' coef(fit, "median", intervals = function(x) {
#'     stats::quantile(x, probs = c(0.025, 0.975))
#' })
#' coef(fit, "mean", "sd")
#' }
coef.JointModelSamples <- function(object, ...) {
    covariate_names <- list()
    if (
        !is.null(object@model@survival) &&
            is(object@data@survival, "DataSurvival")
    ) {
        covariate_names$beta_os_cov <- covariates(object@data@survival)
    }
    if (!is.null(object@model@longitudinal)) {
        covariate_names <- c(
            covariate_names,
            covariates(object@model@longitudinal, data = object@data@subject)
        )
    }

    rows <- lapply(names(covariate_names), function(parameter) {
        covariates <- covariate_names[[parameter]]
        if (length(covariates) == 0L) {
            return(NULL)
        }
        survival <- identical(parameter, "beta_os_cov")
        predictor <- if (survival) {
            NA_character_
        } else {
            sub("_coefficients$", "", sub("^lm_[^_]+_", "", parameter))
        }
        tibble::tibble(
            component = if (survival) "survival" else "longitudinal",
            predictor = predictor,
            covariate = covariates,
            variable = paste0(parameter, "[", seq_along(covariates), "]")
        )
    })
    labels <- dplyr::bind_rows(rows)
    assert_that(
        nrow(labels) > 0L,
        msg = "No covariate coefficients are available for this model."
    )

    estimates <- cmdstanr::as.CmdStanMCMC(object)$summary(
        variables = labels$variable,
        ...
    )
    indices <- match(labels$variable, estimates$variable)
    assert_that(
        !anyNA(indices),
        msg = "Sampled coefficients do not match the model design matrices."
    )
    dplyr::bind_cols(
        labels,
        estimates[indices, setdiff(names(estimates), "variable"), drop = FALSE]
    )
}

#' Extract shrinkage factors from a `JointModelSamples` object
#'
#' Extracts all sampled variables whose names start with
#' `prior_shrinkage_factors_`, including those generated by horseshoe priors on
#' survival and longitudinal coefficients. These are prior shrinkage factors,
#' distinct from the tumour-shrinkage rate in a longitudinal model.
#'
#' Survival coefficient factors retain their covariate names. Longitudinal
#' coefficient factors are labelled `longitudinal.<predictor>:<covariate>`, for
#' example `longitudinal.mu_g:age` or `longitudinal.omega_s:armB`. Their
#' covariate names are obtained from [covariates.LongitudinalModel()]. Factors
#' without a known covariate mapping retain their Stan variable names.
#'
#' Survival-only and longitudinal-only fits are supported. An error is raised
#' if no prior shrinkage factors were sampled or if sampled indices cannot be
#' matched to a known design matrix.
#'
#' @typed object: JointModelSamples
#'   the object to extract shrinkage factors from.
#' @param ... not used.
#' @returns A posterior draws object containing the shrinkage factors, with
#'   iterations, chains, and variable order preserved. It can be summarised
#'   with [posterior::summarise_draws()].
#'
#' @family shrinkage
#' @export
shrinkage.JointModelSamples <- function(object, ...) {
    draws <- cmdstanr::as.CmdStanMCMC(object)$draws()
    prefix <- "prior_shrinkage_factors_"
    variables <- posterior::variables(draws)
    variables <- variables[startsWith(variables, prefix)]
    assert_that(
        length(variables) > 0,
        msg = "No sampled prior shrinkage factors are available."
    )
    shrinkage_draws <- subset(draws, variable = variables)
    parameters <- sub("\\[.*$", "", substring(variables, nchar(prefix) + 1))
    covariate_names <- .shrinkage_covariate_names(object, unique(parameters))
    labels <- variables
    for (parameter in names(covariate_names)) {
        selected <- which(parameters == parameter)
        expected <- paste0(
            prefix,
            parameter,
            "[",
            seq_along(covariate_names[[parameter]]),
            "]"
        )
        indices <- match(variables[selected], expected)
        assert_that(
            !anyNA(indices),
            msg = sprintf(
                "Shrinkage factors for `%s` do not match its design matrix.",
                parameter
            )
        )
        labels[selected] <- covariate_names[[parameter]][indices]
    }
    assert_that(
        !anyDuplicated(labels),
        msg = "Shrinkage factor labels are not unique."
    )
    posterior::variables(shrinkage_draws) <- labels
    shrinkage_draws
}

#' Build covariate labels for sampled shrinkage parameters
#'
#' @param object A [`JointModelSamples`] object supplying model and data.
#' @param parameters Character vector of parameter names, without the
#'   `prior_shrinkage_factors_` prefix or element indices.
#' @returns A named list of covariate labels for recognised parameters.
#'   Unmapped parameters are omitted so the caller can retain their Stan names.
#' @keywords internal
.shrinkage_covariate_names <- function(object, parameters) {
    result <- list()
    if (
        "beta_os_cov" %in% parameters &&
            is(object@data@survival, "DataSurvival")
    ) {
        result$beta_os_cov <- covariates(object@data@survival)
    }

    if (is.null(object@model@longitudinal)) {
        return(result)
    }

    longitudinal_covariates <- covariates(
        object@model@longitudinal,
        data = object@data@subject
    )
    for (parameter in intersect(parameters, names(longitudinal_covariates))) {
        covariate_names <- longitudinal_covariates[[parameter]]
        if (is.null(covariate_names)) {
            next
        }
        predictor <- sub("^lm_[^_]+_", "", parameter)
        predictor <- sub("_coefficients$", "", predictor)
        result[[parameter]] <- paste0(
            "longitudinal.", predictor, ":", covariate_names
        )
    }
    result
}
