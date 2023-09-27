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
    data <- as.list(object@data)
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



# longitudinal-JointModelSamples ----

#' @rdname longitudinal
#'
#' @param patients (`character` or `NULL`)\cr optional subset of patients for
#' which the longitudinal fit samples should be extracted, the default `NULL`
#' meaning all patients.
#'
#' @param time_grid (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#'
#' @param ... Not Used.
#'
#' @export
longitudinal.JointModelSamples <- function(object, patients = NULL, time_grid = NULL, ...) {
    data <- as.list(object@data)
    time_grid <- expand_time_grid(time_grid, max(data[["Tobs"]]))
    patients <- expand_patients(patients, names(data$pt_to_ind))
    gq <- generateQuantities(
        object,
        patients = patients,
        time_grid_lm = time_grid,
        time_grid_sm = numeric(0)
    )

    y_fit_at_grid_samples <- gq$draws(format = "draws_matrix")
    y_fit_samples <- object@results$draws("Ypred", format = "draws_matrix")

    results <- list()
    for (this_pt_ind in seq_along(patients)) {
        this_pt <- patients[this_pt_ind]
        this_result <- list()
        this_y_fit_names <- sprintf(
            "y_fit_at_time_grid[%i,%i]",
            this_pt_ind,
            seq_along(time_grid)
        )
        this_result$samples <- y_fit_at_grid_samples[, this_y_fit_names, drop = FALSE]
        this_result$summary <- data.frame(
            time = time_grid,
            samples_median_ci(this_result$samples)
        )
        for_this_pt <- which(data$ind_index == data$pt_to_ind[this_pt])
        this_fit <- samples_median_ci(y_fit_samples[, for_this_pt, drop = FALSE])
        this_result$observed <- data.frame(
            t = data$Tobs[for_this_pt],
            y = data$Yobs[for_this_pt],
            this_fit
        )
        results[[this_pt]] <- this_result
    }
    .LongitudinalSamples(results)
}
