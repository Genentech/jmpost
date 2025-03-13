
#' Re-used documentation for `RandomEffectQuantities`
#'
#' @param x ([`RandomEffectQuantities`]) \cr generated quantities.
#' @param object ([`RandomEffectQuantities`]) \cr generated quantities.
#' @param conf.level (`numeric`) \cr confidence level of the interval.
#' @param ... not used.
#'
#' @keywords internal
#' @name RandomEffectQuantities-Shared
NULL






#' Random Effects Quantities Container
#'
#' A simple wrapper around a `matrix` to store required metadata for patient level
#' random effects data
#'
#' @param quantities (`matrix`)\cr of random effects values.
#' @param subject (`character`)\cr labels specifying which subjects the values belong to.
#' @param parameter (`character`)\cr labels specifying which parameter the value is.
#'
#' @slot quantities (`matrix`)\cr See Arguments for details.
#' @slot subject (`numeric`)\cr See Arguments for details.
#' @slot parameter (`character`)\cr See Arguments for details.
#'
#' @details
#' Each row of the matrix represents a sample and each column represents a distinct subject
#' specific parameter.
#' As such the number of columns in the matrix should equal the length of `subject` and `parameter`
#' which provide metadata for who the parameter corresponds to as well as which parameter it is.
#'
#' @keywords internal
#' @name RandomEffectQuantities-class
#' @family RandomEffectQuantities
.RandomEffectQuantities <- setClass(
    "RandomEffectQuantities",
    slots = list(
        "quantities" = "matrix",
        "subject" = "character",
        "parameter" = "character"
    )
)
#' @rdname RandomEffectQuantities-class
RandomEffectQuantities <- function(quantities, subject, parameter) {
    .RandomEffectQuantities(
        quantities = quantities,
        subject = subject,
        parameter = parameter
    )
}

setValidity(
    Class = "RandomEffectQuantities",
    method = function(object) {
        if (length(object@subject) != ncol(object@quantities)) {
            return("Length of `subject` must be equal to the number of columns in `quantities`")
        }
        if (length(object@parameter) != ncol(object@quantities)) {
            return("Length of `parameter` must be equal to the number of columns in `quantities`")
        }
        TRUE
    }
)


#' `RandomEffectQuantities` -> Printable `Character`
#'
#' Converts [`RandomEffectQuantities`] object into a printable string.
#' @inheritParams RandomEffectQuantities-Shared
#' @param indent (`numeric`) \cr the number of spaces to indent the string by.
#' @family RandomEffectQuantities
#' @keywords internal
#' @export
as_print_string.RandomEffectQuantities <- function(object, indent = 1, ...) {
    parameter_string <- paste0("        ", unique(object@parameter))
    template <- c(
        "RandomEffectQuantities Object:",
        "    # of samples         = %d",
        "    # of unique subjects = %d",
        "    For parameters:",
        parameter_string
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n"),
        nrow(object@quantities),
        length(unique(object@subject))
    )
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "RandomEffectQuantities",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)



#' `RandomEffectQuantities` -> `data.frame`
#'
#' Returns a `data.frame` of the subject-level random effect parameter samples.
#'
#' @inheritParams RandomEffectQuantities-Shared
#'
#' @keywords internal
#' @family RandomEffectQuantities
#' @export
as.data.frame.RandomEffectQuantities <- function(x, ...) {
    data.frame(
        subject = rep(x@subject, each = nrow(x@quantities)),
        parameter = rep(x@parameter, each = nrow(x@quantities)),
        values = as.vector(x@quantities)
    )
}


#' summary
#'
#' @description
#' This method returns a summary statistic `data.frame` of the random effect parameters
#'
#' @inheritParams RandomEffectQuantities-Shared
#'
#' @returns
#' A `data.frame` with the following variables:
#' - `subject` (`character`) \cr the subject identifier.
#' - `parameter` (`character`) \cr the parameter identifier.
#' - `median` (`numeric`) \cr the median value of the quantity.
#' - `lower` (`numeric`) \cr the lower CI value of the quantity.
#' - `upper` (`numeric`) \cr the upper CI value of the quantity.
#'
#' @keywords internal
#' @family RandomEffectQuantities
#' @export
summary.RandomEffectQuantities <- function(object, conf.level = 0.95, ...) {
    quantities_summarised <- samples_median_ci(
        object@quantities,
        level = conf.level
    )

    quantities_summarised$subject <- object@subject
    quantities_summarised$parameter <- object@parameter
    quantities_summarised[, c("subject", "parameter", "median", "lower", "upper")]
}


#' Extract Random Effects Samples from a Longitudinal Model
#'
#' Helper function to extract subject-level random effects samples from the longitudinal
#' sub-model of a joint model samples object.
#'
#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#' @family RandomEffectQuantities
#' @family JointModelSamples
#' @export
LongitudinalRandomEffects <- function(object) {
    assert_class(object, "JointModelSamples")

    subject_indexes <- as.list(object@data)$subject_to_index
    random_effects_names <- getRandomEffectsNames(object@model)
    expanded <- expand.grid(
        parameter_long = random_effects_names,
        subject_indexes = subject_indexes
    )
    expanded$subject <- names(subject_indexes)[expanded$subject_indexes]
    expanded$parameter <- names(random_effects_names)[expanded$parameter_long]
    stan_parameter_names <- sprintf("%s[%i]", expanded$parameter_long, expanded$subject_indexes)
    draw_matrix <- posterior::as_draws_matrix(object@results$draws(stan_parameter_names))
    class(draw_matrix) <- "matrix"
    rownames(draw_matrix) <- NULL
    colnames(draw_matrix) <- NULL

    RandomEffectQuantities(
        draw_matrix,
        subject = expanded$subject,
        parameter = expanded$parameter
    )
}
