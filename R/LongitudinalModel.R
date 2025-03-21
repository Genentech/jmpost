#' @include StanModel.R
NULL

# LongitudinalModel-class ----

#' `LongitudinalModel`
#'
#' This class extends the general [`StanModel`] class to comprise the longitudinal
#' model specification.
#'
#' @exportClass LongitudinalModel
.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    contains = "StanModel"
)

# LongitudinalModel-constructors ----

#' @rdname LongitudinalModel-class
#'
#' @inheritParams stanmodel_arguments
#'
#' @export
LongitudinalModel <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    ...
) {
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
        )
    )
}

#' @export
as_print_string.LongitudinalModel <- function(object, ...) {
    string <- sprintf(
        "\n%s Longitudinal Model with parameters:\n%s\n\n",
        object@name,
        paste("   ", as_print_string(object@parameters)) |> paste(collapse = "\n")
    )
    return(string)
}
