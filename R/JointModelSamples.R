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

# longitudinal-JointModelSamples ----

#' @rdname longitudinal
#'
#' @param patients (`character` or `NULL`)\cr optional subset of patients for
#' which the longitudinal fit samples should be extracted, the default `NULL`
#' meaning all patients.
#'
#' @export
setMethod(
    f = "longitudinal",
    signature = c(object = "JointModelSamples"),
    definition = function(object, patients = NULL, ...) {
        all_pts <- names(object@data$pt_to_ind)
        if (is.null(patients)) patients <- all_pts
        assert_that(all(patients %in% all_pts), !any(duplicated(patients)))
        time_grid <- object@data$lm_time_grid
        assert_that(is.numeric(object@data$lm_time_grid))
        time_grid_index <- seq_along(time_grid)
        y_fit_at_grid_samples <- object@results$draws("y_fit_at_time_grid")
        y_fit_samples <- object@results$draws("Ypred")
        results <- list()
        for (this_pt in patients) {
            this_result <- list()
            # Samples.
            patient_ind <- object@data$pt_to_ind[this_pt]
            this_y_fit_names <- paste0("y_fit_at_time_grid[", patient_ind, ",", time_grid_index, "]")
            this_result$samples <- y_fit_at_grid_samples[, 1L, this_y_fit_names, drop = TRUE]
            # Summary.
            y_fit <- samples_median_ci(this_result$samples)
            this_result$summary <- cbind(time = time_grid, y_fit)
            # Observations.
            for_this_pt <- which(object@data$ind_index == patient_ind)
            this_t <- object@data$Tobs[for_this_pt]
            this_y <- object@data$Yobs[for_this_pt]
            this_fit <- y_fit_samples[for_this_pt]
            this_result$observed <- data.frame(t = this_t, y = this_y, fit = this_fit)
            # Save all.
            results[[this_pt]] <- this_result
        }
        .LongitudinalSamples(results)
    }
)
