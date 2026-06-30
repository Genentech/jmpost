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

#' @export
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
