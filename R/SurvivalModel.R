#' @include StanModel.R
NULL

# SurvivalModel-class ----

#' `SurvivalModel`
#'
#' This class extends the general [`StanModel`] class to comprise the survival
#' model specification.
#'
#' @exportClass SurvivalModel
.SurvivalModel <- setClass(
    Class = "SurvivalModel",
    contains = "StanModel"
)

# SurvivalModel-constructors ----

#' @rdname SurvivalModel-class
#'
#' @inheritParams stanmodel_arguments
#'
#' @export
SurvivalModel <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    ...
) {
    base_stan <- read_stan("base/survival.stan")
    stan_full <- decorated_render(
        .x = base_stan,
        stan = add_missing_stan_blocks(as.list(stan))
    )
    .SurvivalModel(
        StanModel(
            name = name,
            stan = StanModule(stan_full),
            parameters = parameters,
            ...
        )
    )
}

#' @export
as_print_string.SurvivalModel <- function(object, ...) {
    string <- sprintf(
        "\n%s Survival Model with parameters:\n%s\n\n",
        object@name,
        paste("   ", as_print_string(object@parameters)) |> paste(collapse = "\n")
    )
    return(string)
}
