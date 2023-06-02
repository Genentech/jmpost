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
#' @inheritParams stanmodule_arguments
#'
#' @export
SurvivalModel <- function(stan = StanModule(),
                          parameters = ParameterList(),
                          ...) {
    base_stan <- paste0(read_stan("base/survival.stan"), collapse = "\n")
    stan_full <- jinjar::render(
        .x = base_stan,
        stan = add_missing_stan_blocks(as.list(stan))
    )
    .SurvivalModel(
        StanModel(
            stan = StanModule(stan_full),
            parameters = parameters,
            ...
        )
    )
}
