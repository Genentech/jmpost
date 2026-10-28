#' @include StanModel.R
NULL

# LongitudinalModel-class ----

#' `LongitudinalModel`
#'
#' This class extends the general [`StanModel`] class to comprise the longitudinal
#' model specification.
#'
#' @slot scaled_variance logical scalar, whether the variance should be scaled
#'   by the expected value, corresponding to a multiplicative model. If not,
#'   then an additive error model is used.
#'
#' @exportClass LongitudinalModel
.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    contains = "StanModel",
    slots = c(
        scaled_variance = "logical"
    )
)

# LongitudinalModel-constructors ----

#' @rdname LongitudinalModel-class
#'
#' @inheritParams stanmodel_arguments
#' @typed scaled_variance: flag
#'   whether the variance should be scaled
#'   by the expected value, corresponding to a multiplicative model. If not,
#'   then an additive error model is used.
#'
#' @export
#'
#' @returns A `LongitudinalModel` object.
#'
#' @examples
#' LongitudinalModel(scaled_variance = FALSE)
LongitudinalModel <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    scaled_variance = NA,
    ...
) {
    assert_flag(scaled_variance)
    base_stan <- read_stan("base/longitudinal.stan")

    stan_full <- decorated_render(
        .x = base_stan,
        stan = add_missing_stan_blocks(as.list(stan))
    )

    .LongitudinalModel(
        StanModel(
            stan = StanModule(stan_full),
            parameters = parameters,
            name = name,
            ...
        ),
        scaled_variance = scaled_variance
    )
}

#' Create longitudinal-model-specific Stan data
#'
#' @param model A [`LongitudinalModel`] object.
#' @param subject A [`DataSubject`] object.
#'
#' @keywords internal
#' @returns A named list of model-specific Stan data components.
longitudinal_model_stan_data <- function(model, subject) {
    UseMethod("longitudinal_model_stan_data")
}

#' @rdname longitudinal_model_stan_data
#' @keywords internal
#' @export
longitudinal_model_stan_data.default <- function(model, subject) {
    list()
}

#' Extract covariate names for longitudinal model parameters
#'
#' Uses the subject-level design matrices prepared for fitting the model.
#' Covariate names therefore include factor contrasts, transformations, and
#' interactions, in the same order as the corresponding coefficients.
#' Covariates are taken from subject data, rather than repeated measurements
#' in a [`DataLongitudinal`] object.
#'
#' The default longitudinal-model method recognises design matrices named
#' `<name>_design` paired with declared parameters named `<name>_coefficients`.
#' Custom longitudinal models using another convention can provide their own
#' `covariates()` method with the same return structure.
#'
#' @typed object: LongitudinalModel
#'   the longitudinal model specification.
#' @typed data: DataSubject
#'   the subject data used to construct the model's design matrices.
#' @param ... not used.
#'
#' @returns A named list of character vectors, keyed by coefficient parameter
#'   name. Intercept-only predictors have an empty character vector. Models
#'   without covariate predictors, such as [`LongitudinalSteinFojo`], return
#'   an empty list.
#'
#' @family covariates
#' @export
#'
#' @examples
#' subject <- DataSubject(
#'     data.frame(
#'         id = c("1", "2"),
#'         arm = c("A", "B"),
#'         study = "Study 1",
#'         age = c(50, 60)
#'     ),
#'     "id", "arm", "study"
#' )
#' model <- LongitudinalSteinFojoCov(mu_g_formula = ~ arm + age)
#' covariates(model, data = subject)$lm_sfc_mu_g_coefficients
#' covariates(LongitudinalSteinFojo(), data = subject)
covariates.LongitudinalModel <- function(object, data, ...) {
    assert_class(data, "DataSubject")
    stan_data <- longitudinal_model_stan_data(object, subject = data)
    if (is.null(names(stan_data))) {
        return(list())
    }
    # Covariate predictors pair <name>_design with <name>_coefficients.
    design_names <- names(stan_data)[endsWith(names(stan_data), "_design")]
    parameters <- sub("_design$", "_coefficients", design_names)
    selected <- parameters %in% names(getParameters(object))
    designs <- stan_data[design_names[selected]]
    stats::setNames(
        lapply(designs, function(design) as.character(colnames(design))),
        parameters[selected]
    )
}

#' @export
#'
#' @returns A character vector suitable for printing.
as_print_string.LongitudinalModel <- function(object, ...) {
    string <- sprintf(
        "\n%s Longitudinal Model (%s error) with parameters:\n%s\n\n",
        object@name,
        if (is.na(object@scaled_variance)) {
            # Should not happen but just to be robust against this case.
            "unknown"
        } else if (object@scaled_variance) {
            "multiplicative"
        } else {
            "additive"
        },
        paste("   ", as_print_string(object@parameters)) |>
            paste(collapse = "\n")
    )
    return(string)
}
