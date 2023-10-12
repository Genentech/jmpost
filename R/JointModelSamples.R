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
#' @slot results ([`CmdStanMCMC`])\cr the STAN samples.
#'
#' @aliases JointModelSamples
#' @export
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "DataJoint",
        results = "CmdStanMCMC"
    )
)


#' @rdname generateQuantities
#' @param patients (`character`)\cr explicit vector of patient IDs for whom the
#' generated quantities should be extracted
#' @param time_grid_lm (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @param time_grid_sm (`numeric`)\cr grid of time points to use for providing samples
#'   of the survival model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @export
generateQuantities.JointModelSamples <- function(object, patients, time_grid_lm, time_grid_sm, ...) {
    data <- append(
        as_stan_list(object@data),
        as_stan_list(object@model@parameters)
    )
    data[["n_lm_time_grid"]] <- length(time_grid_lm)
    data[["lm_time_grid"]] <- time_grid_lm
    data[["n_sm_time_grid"]] <- length(time_grid_sm)
    data[["sm_time_grid"]] <- time_grid_sm
    data[["n_pt_select_index"]] <- length(patients)
    data[["pt_select_index"]] <- data$pt_to_ind[patients]
    stanObject <- object@model@stan
    stanObject_data <- merge(
        stanObject,
        StanModule("base/generated_quantities_data.stan")
    )

    model <- compileStanModel(stanObject_data)

    devnull <- utils::capture.output(
        results <- model$generate_quantities(
            data = data,
            fitted_params = object@results$draws()
        )
    )
    return(results)
}
