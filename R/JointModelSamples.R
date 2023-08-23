#' @include JointModel.R
NULL

# JointModelSamples-class ----

#' `JointModelSamples`
#'
#' Contains samples from a [`JointModel`].
#'
#' @slot model (`JointModel`)\cr the original model.
#' @slot data (`list`)\cr data input.
#' @slot init (`list`)\cr initial values.
#' @slot results (`CmdStanMCMC`)\cr the results from [sampleStanModel()].
#'
#' @aliases JointModelSamples
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


#' @rdname generateQuantities
#'
#' @param time_grid_lm (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @param time_grid_os (`numeric`)\cr grid of time points to use for providing samples
#'   of the survival model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @export
setMethod(
    f = "generateQuantities",
    signature = c(object = "JointModelSamples"),
    definition = function(object, time_grid_lm = NULL, time_grid_sm = NULL, ...) {
        data <- object@data
        default_grid <- seq(from = 0, to = max(data[["Times"]]), length = 201)
        if (is.null(time_grid_lm)) {
            time_grid_lm <- default_grid
        }
        if (is.null(time_grid_sm)) {
            time_grid_sm <- default_grid
        }
        validate_time_grid(time_grid_lm)
        validate_time_grid(time_grid_sm)
        data[["n_lm_time_grid"]] <- length(time_grid_lm)
        data[["lm_time_grid"]] <- time_grid_lm
        data[["n_sm_time_grid"]] <- length(time_grid_sm)
        data[["sm_time_grid"]] <- time_grid_sm

        stanObject <- object@model@stan
        stanObject_data <- merge(
            stanObject,
            StanModule("base/generated_quantities_data.stan")
        )
        model <- compileStanModel(stanObject_data)

        results <- model$generate_quantities(
            data = data,
            fitted_params = object@results$draws()
        )
        object@data <- data
        JointModelQuantities(
            object,
            results
        )
    }
)
