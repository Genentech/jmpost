#' @include JointModel.R
NULL

#' Samples from a `JointModel`
#'
#' @slot model (`JointModel`)\cr the original model.
#' @slot data (`list`)\cr data input.
#' @slot init (`list`)\cr initial values.
#' @slot results (`CmdStanMCMC`)\cr the results from [sampleStanModel()].
#' @export
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "list",
        init = "list",
        results = "ANY"
    )
)
